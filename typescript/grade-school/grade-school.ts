type StudentRoster<T> = Map<T, string[]>;

export default class GradeSchool {
  private roster: StudentRoster<number> = new Map();

  addStudent(name: string, grade: number): void {
    const students = (this.roster.get(grade) || []).concat(name);
    this.roster.set(grade, students);
  }

  studentRoster(): StudentRoster<string> {
    const stringRoster: StudentRoster<string> = new Map();

    for (const key of this.roster.keys()) {
      stringRoster.set(key.toString(), this.studentsInGrade(key));
    }

    return stringRoster;
  }

  studentsInGrade(grade: number): string[] {
    return (this.roster.get(grade) || []).sort().slice();
  }
}
