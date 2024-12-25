#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <filesystem>
#include <fstream>
#include <numeric>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using Report = std::vector<int>;
using Reports = std::vector<Report>;

const bool isReportSafe(const Report& report) noexcept {
  const auto pairs = std::ranges::views::pairwise(report);

  const bool allAscending = std::all_of(pairs.cbegin(), pairs.cend(), [](auto p) {
    return std::get<0>(p) < std::get<1>(p);
  });
  const bool allDescending = std::all_of(pairs.cbegin(), pairs.cend(), [](auto p) {
    return std::get<0>(p) > std::get<1>(p);
  });
  const bool isDiffOk = std::all_of(pairs.cbegin(), pairs.cend(), [](auto p) {
    const auto& [l, r] = p;
    const auto diff = std::abs(l - r);
    return 1 <= diff && diff <= 3;
  });

  return (allAscending || allDescending) && isDiffOk;
}


int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "ERROR - Invalid number of arguments: expected 1, got " << (argc - 1) << '\n';
    return EXIT_FAILURE;
  }

  const std::filesystem::path filename = argv[1];
  std::ifstream inputStream(filename);
  
  Reports reports;
  for (std::string line; std::getline(inputStream, line); ) {
    std::istringstream lineStream;
    lineStream.str(line);
    Report report;
    for (std::string str; std::getline(lineStream, str, ' '); ) {
      int number = std::stoi(str);
      report.emplace_back(number);
    }
    reports.emplace_back(report);
  }

  const std::size_t result = std::count_if(reports.cbegin(), reports.cend(), isReportSafe);

  std::cout << result << '\n';

  return 0;
}
