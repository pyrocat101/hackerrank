class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.xans = None
        self.yans = None

def meetingPoint(cx, cy):
    # clockwise rotate 45 degree
    xs = []
    ys = []
    for x, y in zip(cx, cy):
        xs.append(x + y)
        ys.append(x - y)

    points = []
    num_points = len(xs)
    xsum = sum(xs)
    ysum = sum(ys)
    for x, y in zip(xs, ys):
        points.append(Point(x, y))

    # sort by x
    points.sort(cmp=lambda a, b: cmp(a.x, b.x))

    # update left x value
    # for all points <= x
    acc = 0
    for p in points:
        acc += p.x
        p.xans = acc
    # compute distance
    for i, p in enumerate(points):
        left = p.xans
        # right = xsum - left
        right = xsum - left
        # subtract to get distance
        left = (i + 1) * p.x - left
        right = right - (num_points - i - 1) * p.x
        p.xans = left + right

    # sort by y
    points.sort(cmp=lambda a, b: cmp(a.y, b.y))

    # update left y value
    acc = 0
    for p in points:
        acc += p.y
        p.yans = acc
    # compute distance
    for i, p in enumerate(points):
        left = p.yans
        # right = ysum - left
        right = ysum - left
        # subtract to get distance
        left = (i + 1) * p.y - left
        right = right - (num_points - i - 1) * p.y
        p.yans = left + right

    min_dist = points[0].xans + points[0].yans
    for p in points:
        if min_dist > p.xans + p.yans:
            min_dist = p.xans + p.yans

    return min_dist / 2

if __name__ == '__main__':
    a = input()
    x=[0]*a
    y=[0]*a
    for i in range(0,a):
        b = map(int, raw_input().strip().split(" "))
        x[i]=b[0]
        y[i]=b[1]
    print meetingPoint(x,y)
