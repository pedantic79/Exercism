from typing import List, Dict

DEFAULT_STUDENTS: List[str] = [
    "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Fred",
    "Ginny",
    "Harriet",
    "Ileana",
    "Joseph",
    "Kincaid",
    "Larry",
]
PLANT_NAMES: Dict[str, str] = {
    plant[0]: plant for plant in ["Clover", "Radishes", "Grass", "Violets"]
}


class Garden:
    def __init__(self, diagram: str, students: List[str] = []):
        self.garden: List[str] = diagram.splitlines()
        self.students: List[str] = sorted(
            students if students else DEFAULT_STUDENTS
        )

    def plants(self, student: str) -> List[str]:
        student_index: int = self.students.index(student)
        start_index: int = student_index * 2
        end_index: int = start_index + 2

        return [
            PLANT_NAMES[abbreviation]
            for row in self.garden
            for abbreviation in row[start_index:end_index]
        ]
