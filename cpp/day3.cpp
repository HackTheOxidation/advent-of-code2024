#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <numeric>
#include <regex>
#include <sstream>
#include <string>


int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "ERROR - Invalid number of arguments: expected 1, got " << (argc - 1) << '\n';
    return EXIT_FAILURE;
  }

  const std::filesystem::path filename = argv[1];
  std::ifstream inputStream(filename);
  inputStream >> std::noskipws;

  std::regex mulRegex("(mul\\((\\d+),(\\d+)\\))|(do\\(\\))|(don't\\(\\))", std::regex_constants::ECMAScript | std::regex_constants::icase);
  
  std::size_t sum = 0;
  std::string line; 
  bool enabled = true;
  while(std::getline(inputStream, line)) {
    auto mulBegin = std::sregex_iterator(line.begin(), line.end(), mulRegex);
    const auto mulEnd = std::sregex_iterator();
    for (std::sregex_iterator i = mulBegin; i != mulEnd; i++) {
      std::smatch sm = *i; 
      auto op = sm[0];
      if (op == "do()") {
        enabled = true;
      } else if (op == "don't()") {
        enabled = false;
      } else if (enabled) {
        sum += std::stold(sm[2]) * std::stold(sm[3]);
      }
    }
  }

  std::cout << sum << '\n';

  return 0;
}
