# -*- mode: snippet -*-
# name: meeting
# key: meeting
# contributor: FR Lyvers <verslyfr@github.com>
# --

## `(format-time-string "%Y-%m-%d")` ${1:topic}

- **Attendees:** ${2:People separated by ;}

| **P**: ${Purpose}
| **O**: ${Outcome}
| **S**: Agenda

1. ${topic} - ${person} (${min} min)
2. ${topic} - ${person} (${min} min)

| **T**: ${duration}

Project: +${project}

## dd$0 $1

- *Attendees:* $2
- *Actions:*
- *Decisions:*
- *Notes:*
-
