#include <spdlog/spdlog.h>
#include "spdlog/sinks/stdout_color_sinks.h"

#include "sqlhandler.h"

int main(int argc, char *argv[]) {
  auto consoleLog = spdlog::stdout_color_mt("console");

  // Parse the command line options

  consoleLog->info("Starting up the metalogger...");

  SQLHandler sqlHandler("/tmp/metalogger.db");

  consoleLog->info("Shutting down the metalogger...");
}