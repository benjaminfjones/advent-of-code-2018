import re
import datetime


test_data_s = """\
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

test_data = test_data_s.split('\n')


def parse_line(inp):
    """ Returns (year,month,day,hour,min,action) """
    r = re.match(r'\[(\d+)\-(\d+)\-(\d+) (\d+):(\d+)\] (.+)', inp)
    t = list(r.groups())
    t[:5] = map(int, t[:5])

    i = re.match(r'Guard #(\d+) begins shift', t[5])
    if i:
        t6 = {'Guard': int(i.group(1))}
    elif re.match(r'falls asleep', t[5]):
        t6 = {'falls asleep': True}
    elif re.match(r'wakes up', t[5]):
        t6 = {'wakes up': True}
    else:
        print('foo')
        raise Exception('foobar')

    t.append(t6)
    return t


def organize(xs):
    m = {}
    delta_day = datetime.timedelta(1)
    for x in xs:
        og = datetime.datetime(*x[:5])
        dt = datetime.datetime(*x[:5])
        if dt.hour == 23:
            dt = dt + delta_day
            dt.replace(hour=0)
            print(f'replaced {og} with {dt}')
        ymd = (x[0], x[1], x[2])
        if ymd not in m:
            m[ymd] = x[3:]
        else:
            m[ymd].append(x[3:])

    # fix up the early guard arrivals

    # sort events for the day
    return m


def main():
    xs = map(parse_line, test_data)
    print(organize(xs))
