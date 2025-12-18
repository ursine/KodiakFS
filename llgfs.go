package main

import (
	"context"
	"fmt"
	"os/signal"
	"syscall"

	"log/slog"

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
