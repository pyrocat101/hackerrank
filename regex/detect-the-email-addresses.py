import re

em = re.compile("^([0-9a-zA-Z](?:[-\.\w]*[0-9a-zA-Z])*@(?:[0-9a-zA-Z][-\w]*[0-9a-zA-Z]\.)+[a-zA-Z]{2,9})")

if __name__ == '__main__':
    line_number = int(raw_input())
    lines = []
    for i in range(line_number):
        lines.append(raw_input())

    # filter lines without @
    lines = filter(lambda l: '@' in l, lines)

    # split into words
    words_in_lines = map(lambda l: l.split(), lines)
    words = [word for sublist in words_in_lines for word in sublist]

    # filter match
    matches = map(lambda w: em.match(w), words)
    found = filter(lambda m: m, matches)
    found = map(lambda m: m.groups()[0], found)
    found = list(set(found))

    # sort
    found.sort()

    print ';'.join(found)
