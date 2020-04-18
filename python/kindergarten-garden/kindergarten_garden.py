DEFAULT_STUDENTS = [
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
PLANT_NAMES = {
    plant[0]: plant for plant in ["Clover", "Radishes", "Grass", "Violets"]
}


class Garden:
    def __init__(self, diagram, students=None):
        self.garden = diagram.splitlines()
        self.students = sorted(students if students else DEFAULT_STUDENTS)

    def plants(self, student):
        student_index = self.students.index(student)
        start_index = student_index * 2
        end_index = start_index + 2

        return [
            PLANT_NAMES[abbreviation]
            for row in self.garden
            for abbreviation in row[start_index:end_index]
        ]
