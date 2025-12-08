//
// Created by bear on 12/8/25.
//

#pragma once
#include <sstream>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include <boost/variant.hpp>
#include "sqlite3.h"

using RowTypes = boost::variant<int64_t, double, std::string, std::vector<char>, std::nullptr_t, int>;
using Row      = std::map<std::string, RowTypes>;
using Rows     = std::vector<Row>;


/**
 * @brief A class representing runtime exceptions from sqlite3
 */
class SqliteException : public std::runtime_error
{
  static std::string buildError(sqlite3* const db, const std::string& msg = "")
  {
    const int err = sqlite3_errcode(db);
    const std::string errStr( sqlite3_errmsg(db) );
    std::ostringstream stream;
    stream << errStr << " [" << err << "] : " << msg;
    return stream.str();
  }

public:
  explicit SqliteException(sqlite3* const db, const std::string& msg = "")
      : std::runtime_error( buildError(db,msg) ) {}

  static void throwIf(const bool b, sqlite3* const db,
                      const std::string& loc, const std::string& msg = "")
  {
    if (b) {
      std::ostringstream str;
      str << loc << " : " << msg;
      throw SqliteException(db, str.str());
    }
  }
};

class SqlStatement
{
private:
  sqlite3_stmt* stmt;
  std::string query;
  sqlite3* db;
  size_t substitute;

  // Variadic tempates for binding
  // Terminate the recursion
  void bind(const int last) { substitute = last-1; }


};




class SQLHandler {
  std::string dbFile;
  sqlite3* database;

public:
  explicit SQLHandler(std::string file) :
          dbFile(std::move(file)), database(nullptr)
  {
    const int retval = sqlite3_open_v2(dbFile.c_str(), &database,
      SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_EXRESCODE, nullptr);
    SqliteException::throwIf(retval!=SQLITE_OK, database, __PRETTY_FUNCTION__, "Unable to open");
  }

  [[nodiscard]] std::string getFileName() const {
    return dbFile;
  }


};
