package main

import (
	"context"
	"encoding/binary"
	"fmt"
	"io"
	"io/ioutil"
	"os/signal"
	"path/filepath"
	"syscall"

	"log/slog"

	"github.com/lni/dragonboat/v3"
	"github.com/lni/dragonboat/v3/config"
	sm "github.com/lni/dragonboat/v3/statemachine"
	"github.com/ursine/KodiakFS/cmd"
	"github.com/ursine/KodiakFS/src/gfs/chunkserver"
	"github.com/ursine/KodiakFS/src/gfs/master"

	"os"

	"github.com/ursine/KodiakFS/src/gfs"
)

func runMaster(_ context.Context) {
	if len(os.Args) < 4 {
		printUsage()
		return
	}
	addr := gfs.ServerAddress(os.Args[2])
	master.NewAndServe(addr, os.Args[3])

	ch := make(chan bool)
	<-ch
}

func runChunkServer(_ context.Context) {
	if len(os.Args) < 5 {
		printUsage()
		return
	}
	addr := gfs.ServerAddress(os.Args[2])
	serverRoot := os.Args[3]
	masterAddr := gfs.ServerAddress(os.Args[4])
	chunkserver.NewAndServe(addr, masterAddr, serverRoot)

	ch := make(chan bool)
	<-ch
}

func printUsage() {
	fmt.Println("Usage:")
	fmt.Println("  gfs master <addr> <root path>")
	fmt.Println("  gfs chunkserver <addr> <root path> <master addr>")
	fmt.Println()
}

func main() {
	// Create a context that cancels when an interrupt or SIGTERM is received.
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	programLevel := new(slog.LevelVar)

	h := slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: programLevel})
	slog.SetDefault(slog.New(h))

	cmd.Execute()

	// Dragonboat config
	rc := config.Config{
		ClusterID:          1,
		NodeID:             1,
		ElectionRTT:        10,
		HeartbeatRTT:       1,
		CheckQuorum:        true,
		SnapshotEntries:    10,
		CompactionOverhead: 5,
	}

	datadir := filepath.Join(
		"/tmp",
		"testing",
		fmt.Sprintf("node%d", rc.NodeID))

	nhc := config.NodeHostConfig{
		WALDir:         datadir,
		NodeHostDir:    datadir,
		RTTMillisecond: 200,
		RaftAddress:    "localhost:63001",
	}

	nh, err := dragonboat.NewNodeHost(nhc)
	if err != nil {
		panic(err)
	}
	defer nh.Stop()

	initialMembers := make(map[uint64]string)

	if err := nh.StartCluster(initialMembers, true, NewExampleStateMachine, rc); err != nil {
		fmt.Fprintf(os.Stderr, "failed to add cluster, %v\n", err)
		os.Exit(1)
	}

	if len(os.Args) < 2 {
		printUsage()
		return
	}

	switch os.Args[1] {
	case "master":
		runMaster(ctx)
	case "chunkserver":
		runChunkServer(ctx)
	default:
		printUsage()
	}
}

type ExampleStateMachine struct {
	ClusterID uint64
	NodeID    uint64
	Count     uint64
}

// NewExampleStateMachine creates and return a new ExampleStateMachine object.
func NewExampleStateMachine(clusterID uint64,
	nodeID uint64) sm.IStateMachine {
	return &ExampleStateMachine{
		ClusterID: clusterID,
		NodeID:    nodeID,
		Count:     0,
	}
}

func (s *ExampleStateMachine) Lookup(query interface{}) (interface{}, error) {
	result := make([]byte, 8)
	binary.LittleEndian.PutUint64(result, s.Count)
	return result, nil
}

func (s *ExampleStateMachine) Update(data []byte) (sm.Result, error) {
	// in this example, we print out the following hello world message for each
	// incoming update request. we also increase the counter by one to remember
	// how many updates we have applied
	s.Count++
	fmt.Printf("from ExampleStateMachine.Update(), msg: %s, count:%d\n",
		string(data), s.Count)
	return sm.Result{Value: uint64(len(data))}, nil
}

func (s *ExampleStateMachine) SaveSnapshot(w io.Writer,
	fc sm.ISnapshotFileCollection, done <-chan struct{}) error {
	// as shown above, the only state that can be saved is the Count variable
	// there is no external file in this IStateMachine example, we thus leave
	// the fc untouched
	data := make([]byte, 8)
	binary.LittleEndian.PutUint64(data, s.Count)
	_, err := w.Write(data)
	return err
}

func (s *ExampleStateMachine) RecoverFromSnapshot(r io.Reader,
	files []sm.SnapshotFile,
	done <-chan struct{}) error {
	// restore the Count variable, that is the only state we maintain in this
	// example, the input files is expected to be empty
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return err
	}
	v := binary.LittleEndian.Uint64(data)
	s.Count = v
	return nil
}

func (s *ExampleStateMachine) Close() error { return nil }
