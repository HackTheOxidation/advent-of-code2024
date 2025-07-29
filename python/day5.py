import sys
from dataclasses import dataclass
from functools import cmp_to_key
from itertools import combinations

from lark import Lark, Transformer


class PageOrderingTransformer(Transformer):
    def number(self, n):
        (n,) = n
        return int(n)

    page_orderings = set
    page_ordering = tuple


class ManualUpdateTransformer(Transformer):
    def number(self, n):
        (n,) = n
        return int(n)

    manual_updates = list
    manual_update = list


@dataclass(frozen=True)
class SafetyManual:
    page_orderings: set[tuple[int, int]]
    manual_updates: list[list[int]]

    @staticmethod
    def parse_safety_manual(manual_file: str, do_print: bool = False):
        safety_manual_grammar = Lark.open("safety_manuals.lark", start=["safety_manual", "page_orderings", "manual_updates"], parser="lalr")
        with open(manual_file, "r") as input_file:
            page_orderings, manual_updates = input_file.read().split("\n\n")
            parsed_page_orderings = safety_manual_grammar.parse(page_orderings, start="page_orderings")
            parsed_manual_updates = safety_manual_grammar.parse(manual_updates, start="manual_updates")

            if do_print:
                print(f"Page Orderings: {parsed_page_orderings.pretty()}\n")
                print(f"Manual Updates: {parsed_manual_updates.pretty()}\n")

            page_orderings = PageOrderingTransformer().transform(parsed_page_orderings)
            manual_updates = ManualUpdateTransformer().transform(parsed_manual_updates)

            return SafetyManual(page_orderings, manual_updates) 

    def valid_updates(self) -> int:
        def _filter_updates():
            for update in self.manual_updates:
                if all((y, x) not in self.page_orderings for x, y in combinations(update, 2)):
                    yield update

        return sum(update[len(update) // 2] for update in _filter_updates())

    def correct_invalid_updates(self) -> int:
        def _filter_updates():
            for update in self.manual_updates:
                if any((y, x) in self.page_orderings for x, y in combinations(update, 2)):
                    yield update

        def _ordering(x, y):
            return 1 if (y, x) in self.page_orderings else -1

        corrected_updates = (sorted(update, key=cmp_to_key(_ordering)) for update in _filter_updates())

        return sum(update[len(update) // 2] for update in corrected_updates)


def main():
    safety_manual = SafetyManual.parse_safety_manual(sys.argv[1])
    print(safety_manual.valid_updates())
    print(safety_manual.correct_invalid_updates())


if __name__ == '__main__':
    main()
