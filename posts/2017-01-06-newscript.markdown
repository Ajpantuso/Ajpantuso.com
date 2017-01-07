---
title: Newscript Script
author: Andrew Pantuso
photos:
tags: ComputerScience
---
Here is a script for generating a script template.
<!--more-->

This script helps keep new scripts consistent and could also serve as a
reference for good scripting practice.

## Script

``` { .bash .numberLines }
#!/bin/bash
#
# Name: newscript
# Author: Andrew Pantuso
# Description:
# Generates new scripts with heading, description, author,
# error function, and main function

#Globals
readonly NO_NAME_GIVEN=1
readonly INVALID_TARGET_DIR=2
readonly FILE_EXISTS=3

err() {
  echo "[$(date +'%Y-%m-%d %T %z')]: $@" >&2
}

display_help() {
cat <<EOF
$(basename "$0") generates scripts with a standard script layout
Arguments:
NAME  - name of script to be created
[DIR] - target directory where script will be created
Options:
-d    - takes a description of the script as an argument
-a    - takes an author name of the script as an argument
-h    - displays this help script
EOF
}

###########################################################
# Breaks description into lines with a maximum length of 78
# Globals:
#   None
# Arguments:
#  description
# Returns:
#   array of lines containing description text
###########################################################
nice_description() {
  local desc
  desc="$1"
  local c
  local result
  result=''

  if [[ ${#desc} -le 78 ]]; then
    result="# ${desc}"
  else
    while [[ ${#desc} -gt 78 ]]; do
      for (( pos=77; pos>=0; pos-- )); do
        if [[ "${desc:${pos}:1}" =~ [[:space:]] ]]; then
            if [[ -z "${result}" ]]; then
              result="# ${desc:0:$((${pos}+ 1 ))}"
            else
              result="${result}
# ${desc:0:$((${pos}+ 1 ))}"
            fi
          desc="${desc:$((${pos} + 1))}"
          break
        fi
        if [[ ${pos} -eq 0 ]]; then
          result="${result}
# ${desc}"
          desc=''
          break
        fi
       done
     done
  fi
  result="${result}
# ${desc}"
  echo "${result}"
}

####################################################
# Writes the intended script with necessary elements
# Globals:
#   None
# Arguments:
#   name
#   author
#   description
# Returns:
#   None
####################################################
write_file() {
  local name
  name="$1"
  local author
  author="$2"
  local description
  description="$3"

cat <<EOF >> "${name}"
#!/bin/bash
#
# Name: ${name}
# Author: ${author}
# Description:
$(nice_description "${description}")

err() {
  echo "[\$(date +'%Y-%m-%d %T %z')]: \$@" >&2
}

main() {

}

main "\$@"
EOF

}

main() {
  
  local name
  name=''
  local targetDir
  targetDir="$(pwd)"
  local description
  description=''
  local author
  author="$(whoami)"
  local opts
  local OPTIND

  while getopts ':d:a:h' opt; do
    case "${opt}" in
      d) description="${OPTARG}" ;;
      a) author="${OPTARG}" ;;
      h) 
        display_help 
        exit 0
        ;;
      *)
        err "Invalid option ${opt}"
        echo "Usage: $(basename "$0") [-dah] [ARGS] NAME [DIR]"
        ;;
    esac
  done
  shift $((OPTIND - 1))

  if [[ -z "$1" ]]; then
    err "No script name given"
    echo "Try $(basename "$0") -h for help"
    exit "${NO_NAME_GIVEN}"
  fi
  name="$1"

  if [[ -n "$2" ]]; then
    if [[ -d "$2" ]]; then
      targetDir="$2" else
      err "$2 not a directory"
      echo "Usage: $(basename "$0") [-dah] [ARGS] NAME [DIR]"
      exit "${INVALID_TARGET_DIR}"
    fi
  fi

  cd "${targetDir}"

  if [[ -e "${name}" ]]; then
    err "${name} already exists"
    exit "${FILE_EXISTS}"
  else
    write_file "${name}" "${author}" "${description}"
  fi

}

main "$@"
```
---
