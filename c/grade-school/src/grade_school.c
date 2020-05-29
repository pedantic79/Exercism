#include "grade_school.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static roster_t roster = {0};

static int student_cmp(const void *lhs, const void *rhs) {
    student_t *s1 = (student_t *)lhs;
    student_t *s2 = (student_t *)rhs;

    if (s1->grade != s2->grade) {
        return s1->grade - s2->grade;
    }
    return strcmp(s1->name, s2->name);
}

roster_t get_roster() { return roster; }

void clear_roster() { memset(&roster, 0, sizeof(roster_t)); }

roster_t get_grade(uint8_t grade) {
    roster_t grade_roster = {0};

    for (size_t i = 0; i < roster.count; i++) {
        uint8_t student_grade = roster.students[i].grade;
        if (student_grade == grade) {
            grade_roster.students[grade_roster.count].grade = student_grade;
            grade_roster.students[grade_roster.count].name =
                roster.students[i].name;

            grade_roster.count++;
        }
    }

    return grade_roster;
}

bool add_student(char *name, uint8_t grade) {
    roster.students[roster.count].grade = grade;
    roster.students[roster.count].name = name;

    roster.count++;
    qsort(roster.students, roster.count, sizeof(student_t), student_cmp);

    return true;
}
