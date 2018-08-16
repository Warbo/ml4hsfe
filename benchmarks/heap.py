from os import getenv

def track_max_heap():
    """Parse heap profile and return the largest measured value."""
    lines = []
    with open(getenv('profiled'), 'r') as f:
        lines = f.read().split('\n')
    assert len(lines) > 0, repr({
        'error'   : 'No lines read from heap profile',
        'profile' : getenv('profiled'),
    })
    largest = 0
    current = None
    for line in lines:
        if 'BEGIN_SAMPLE' in line:
            current = 0
            continue
        if 'END_SAMPLE' in line:
            if current > largest:
                largest = current
            continue

        if current is None or line == '':
            # Skip empty lines and preamble
            continue

        bits = line.split('\t')
        assert len(bits) == 2, repr({
            'error' : 'Line is not name/size pair',
            'line'  : line
        })

        current += int(bits[1])
    return largest
track_max_heap.unit = "bytes"
