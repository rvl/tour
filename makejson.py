#!/usr/bin/python3

import json
from data import data
import sys
from collections import OrderedDict

def make_json(outf):
    ob = OrderedDict()
    for day in data:
        ob[day.strdate()] = day.get_dict()

    json.dump(ob, outf)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        outf = open(sys.argv[1], "w")
    else:
        outf = sys.stdout

    make_json(outf)
