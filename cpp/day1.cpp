#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <filesystem>
#include <fstream>
#include <numeric>
#include <ranges>
#include <string_view>
#include <vector>


int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "ERROR - Invalid number of arguments: expected 1, got " << (argc - 1) << '\n';
    return -1;
  }

  const std::filesystem::path filename = argv[1];
  std::ifstream inputStream(filename);

  std::vector<int> leftVector;
  std::vector<int> rightVector;
  std::size_t counter = 0;

  while (!inputStream.fail()) {
    int input = 0;
    inputStream >> input;

    if (inputStream.eof()) {
      break;
    }
    
    if (counter % 2 == 0) {
      leftVector.emplace_back(input);
    } else {
      rightVector.emplace_back(input);
    }
    counter++;
  }

  std::sort(leftVector.begin(), leftVector.end());
  std::sort(rightVector.begin(), rightVector.end());

  auto subtractAbs = [](const auto l, const auto r) { return std::abs(l - r); };
  const auto diffs = std::views::zip_transform(subtractAbs, leftVector, rightVector);
  const int result = std::reduce(diffs.cbegin(), diffs.cend(), 0);

  std::cout << result << '\n';

  auto countOccurance = [&](auto acc, auto& elem) {
    const auto counts = std::count(rightVector.cbegin(), rightVector.cend(), elem);
    return acc + (elem * counts);
  };

  const int result2 = std::accumulate(leftVector.cbegin(), leftVector.cend(), 0, countOccurance);

  std::cout << result2 << '\n';

  return 0;
}
