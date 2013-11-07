import sys, re

http = r"https?:\/\/((?:[\-a-zA-Z0-9]+\.)*[\-a-zA-Z0-9]+\.[a-zA-Z0-9]+(?:[a-zA-Z0-9]+)?)"

text = sys.stdin.read()
names = re.findall(http, text)
# sort
names = map(lambda x: re.sub(r"^ww[w\d]\.", "", x), names)
names = list(set(names))
names = sorted(names)
print ";".join(names)
