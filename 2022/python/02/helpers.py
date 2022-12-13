class Part:
    total_pts = 0

    def __init__(self, round_outcome_pts, play_pts) -> None:
        self.round_outcome_pts = round_outcome_pts
        self.play_pts = play_pts

part_1 = Part(
    round_outcome_pts = {
        ('A','X'): 3,
        ('A','Y'): 6,
        ('A','Z'): 0,
        ('B','X'): 0,
        ('B','Y'): 3,
        ('B','Z'): 6,
        ('C','X'): 6,
        ('C','Y'): 0,
        ('C','Z'): 3
    },
    play_pts = {
        'X': 1,
        'Y': 2,
        'Z': 3
    }
)
part_2 = Part(
    round_outcome_pts = {
        ('A','X'): 3,
        ('A','Y'): 1,
        ('A','Z'): 2,
        ('B','X'): 1,
        ('B','Y'): 2,
        ('B','Z'): 3,
        ('C','X'): 2,
        ('C','Y'): 3,
        ('C','Z'): 1
    },
    play_pts = {
        'X': 0,
        'Y': 3,
        'Z': 6
    }
)
