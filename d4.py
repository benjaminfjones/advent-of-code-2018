import datetime
import pprint
import re
from typing import List, Dict, Tuple, Union

# test data
test_data_s: str = """\
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"""

test_data: List[str] = test_data_s.split('\n')

#
# Handy Types
#

EventTime = datetime.datetime  # incl. ymdhm
Day = datetime.datetime        # only  ymd
# 0 = asleep, 1 = awake
GuardState = int
GuardID = int
# action strings -> either int or bool (depending on key)
ParsedAction = Dict[str, Union[GuardID, str]]
# date/time -> action that happened starting then
ParsedEvent = Tuple[EventTime, ParsedAction]


def parse_line(inp: str) -> ParsedEvent:
    """ Returns (year,month,day,hour,min,action) """
    r = re.match(r'\[(\d+)\-(\d+)\-(\d+) (\d+):(\d+)\] (.+)', inp)
    t: List[str] = list(r.groups())
    ymd: List[int] = map(lambda x: int(x), t[:5])
    dt = datetime.datetime(*ymd)

    i = re.match(r'Guard #(\d+) begins shift', t[5])
    if i:
        p = {'Guard': int(i.group(1))}
    elif re.match(r'falls asleep', t[5]):
        p = {'falls asleep': True}
    elif re.match(r'wakes up', t[5]):
        p = {'wakes up': True}
    else:
        raise Exception('foobar')

    return (dt, p)


def organize(xs: List[ParsedEvent]) -> \
            Dict[datetime.datetime, List[ParsedEvent]]:
    res = {}
    delta_day = datetime.timedelta(1)
    for x in xs:
        # fix hour 23 guard events to start at next day 00:00
        dt = x[0]
        if dt.hour == 23:
            dt = dt + delta_day
            dt = dt.replace(hour=0, minute=0)
        if dt not in res:
            res[dt] = x
        else:
            res[dt].append(x)
    return res


def process_events(xs: Dict[EventTime, List[ParsedEvent]]) -> \
                   Tuple[GuardID, Dict[int, GuardState]]:
    gid: int = -1
    last_time: int = 0
    for (dt, ys) in xs.items():
        for a, v in ys:
            if a == 'Guard':
                gid = v
            elif a == 'falls asleep':


def main():
    xs = map(parse_line, test_data)
    pprint.pprint(organize(xs))
