import sys, re

fuck_c = map(re.compile,
             [r"^\s*using\s+"])
fuck_java = map(re.compile,
                [r"^\s*public\s+class\s*",
                 r"^\s*import\s+.+\*\s*;",
                 r"throws\s+\w+",
                 r"extends\s+\w+",
                 r"implements\s+\w+",
                 r"catch\s*\("])
fuck_python = map(re.compile,
                  [r"^\s*def\s+.+:",
                   r"^\s*class\s+.+:",
                   r"^\s*if\s+.+:",
                   r"^\s*for\s+.+:",
                   r"^\s*print\s+"])
fuck_c_comment = [re.compile(r"/\*.*?\*/", re.DOTALL), re.compile(r"//.*?\n")]

def main():
    src = sys.stdin.read()

    # eat comments
    for r in fuck_c_comment:
        src = r.sub('', src)

    src = src.split('\n')

    # fuck Java
    for line in src:
        for r in fuck_java:
            if r.match(line):
                print "Java"
                return

    # fuck Python
    for line in src:
        for r in fuck_python:
            if r.match(line):
                print "Python"
                return

    print "C"

if __name__ == '__main__':
    main()
