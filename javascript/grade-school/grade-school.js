export class GradeSchool {
  constructor() {
    this.roll = {}
  }

  roster() {
    return Object.keys(this.roll).reduce((acc, gradeNum) => {
      return { ...acc, [gradeNum]: this.grade(gradeNum) }
    }, {})
  }

  add(name, gradeNum) {
    this.roll[gradeNum] = (this.roll[gradeNum] || []).concat(name).sort()
  }

  grade(gradeNum) {
    return [...(this.roll[gradeNum] || [])]
  }
}
