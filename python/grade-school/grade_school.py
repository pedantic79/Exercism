from typing import Dict, List
from collections import defaultdict


class School:
    def __init__(self) -> None:
        self.student_roster: Dict[int, List[str]] = defaultdict(list)

    def add_student(self, name: str, grade: int) -> None:
        self.student_roster[grade].append(name)
        self.student_roster[grade].sort()

    def roster(self) -> List[str]:
        return [
            name
            for grade in sorted(self.student_roster.keys())
            for name in self.student_roster[grade]
        ]

    def grade(self, grade_number: int) -> List[str]:
        return self.student_roster[grade_number]
