#!/usr/bin/env python

from enum import Enum
from typing import List, Optional


class OptionNameType(Enum):
    SHORT = 1
    LONG = 2
    OLD = 3


class OptionArgType(Enum):
    NOARG = 1
    FIND_FILE = 2
    CHOOSE_FROM  = 3    # <-- either fish array or a fish function returning an array
    DONT_FIND_FILE = 4
    SOME_ARG = 5


class Option(object):
    def __init__(self, names: List[str], argtype: OptionArgType, desc: str, choose_from: Optional[str]=None):
        self.names = names
        self.argtype = argtype
        self.desc = desc
        self.choose_from = choose_from

        if argtype == OptionArgType.CHOOSE_FROM:
            assert self.choose_from is not None

    def as_fish_complete_arguments(self) -> str:
        acc = []

        for dashed in self.names:
            type_ = Option.get_type(dashed)
            if type_ == OptionNameType.LONG:
                opt = "-l"
            elif type_ == OptionNameType.SHORT:
                opt = "-s"
            else:
                assert type_ == OptionNameType.OLD
                opt = "-o"
            bare = dashed.lstrip("-")
            s = f"{opt} {bare}"
            acc.append(s)

        if self.desc:
            s = f"-d '{self.desc}'"
            acc.append(s)

        if self.argtype == OptionArgType.NOARG:
            pass
        elif self.argtype == OptionArgType.FIND_FILE:
            acc.append("-F")
        elif self.argtype == OptionArgType.CHOOSE_FROM:
            s = f'-x -a "{self.choose_from}"'
            acc.append(s)
        elif self.argtype == OptionArgType.DONT_FIND_FILE:
            acc.append("-x")
        else:
            assert self.argtype == OptionArgType.SOME_ARG
            acc.append("-r")

        options_str = " ".join(acc)
        return options_str

    @classmethod
    def get_type(cls, dashed_name: str) -> OptionNameType:
        if dashed_name.starswith("--"):
            res = OptionNameType.LONG
        else:
            assert dashed_name.starswith("-")
            if len(dashed_name[1:]) == 1:
                res = OptionNameType.SHORT
            else:
                res = OptionNameType.OLD
        return res


def set_fish_option(name: str, option: Option) -> str:
    options_str = option.as_fish_complete_arguments()
    s = f"""complete -c {name} {options_str}"""
    return s


def set_fish_subcommand(name: str, subcommand: str, desc: str) -> str:
    s = f"""complete -x -c {name} -n __fish_use_subcommand -a {subcommand} -d '{desc}'"""
    return s


def set_fish_subcommand_options(name: str, subcommand: str, option: Option) -> str:
    """Returns fish completion command suitable for subcommand selection"""
    options_str = option.as_fish_complete_arguments()
    s = f"""complete -c {name} -n "__fish_seen_subcommand_from {subcommand}" {options_str}"""
    return s



def parse_samtools_subc

def parse_samtools_subcommands(lines) -> list(tuple(str, str)):
    for idx, line in enumerate(lines):
        if line.strip().lower().startswith("commands:"):
            break
    else:
        idx = 0
    lines = lines[idx + 1 :]
    pairs = [
        line.strip().split()
        for line in lines
        if line.strip() and (not line.strip().startswith("--"))
    ]
    return pairs


def parse_samtools_subcommand_options(lines) -> list(Option):
    return []


def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("file")
    args = parser.parse_args()
    with open(args.file, "r") as f:
        lines = f.readlines()

    pairs = parse_samtools_subcommands(lines)
    for subcmd, *desc in pairs:
        desc = " ".join(desc)
        s = set_fish_subcommand("samtools", subcmd, desc)
        print(s)


if __name__ == "__main__":
    main()
