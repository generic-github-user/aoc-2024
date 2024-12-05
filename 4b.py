import numpy as np
from pathlib import Path

a = np.lib.stride_tricks.sliding_window_view(np.array([list(l) for l in Path('./aoc-2024/4.txt').read_text().splitlines()]), (3, 3)).reshape((-1, 3, 3))
b = np.where(np.arange(1, 10).reshape((3, 3)) % 2 == 1, a, '-')
print(np.logical_and.reduce((b[:, 1, 1] == 'A', np.sum(b == 'M', axis=(1, 2)) == 2, np.sum(b == 'S', axis=(1, 2)) == 2, np.any(b != np.flip(b, [1, 2]), axis=(1, 2)))).sum())
