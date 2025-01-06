#!/bin/bash

RESET='\033[0m'
BOLD='\033[1m'
RED='\033[31m'
GREEN='\033[32m'
YELLOW='\033[33m'

print_bold() {
  echo -e "${BOLD}$1${RESET}"
}

print_green() {
  echo -e "${GREEN}$1${RESET}"
}

print_yellow() {
  echo -e "${YELLOW}$1${RESET}"
}

print_red() {
  echo -e "${RED}$1${RESET}"
}

# add.mrl
print_bold "Running add.mrl..."
./glados test/test_files/add.mrl
output=$(cat out.s)
print_yellow "got '$output'"
correction=$(cat test/test_files/add.txt)
if [ "$output" == "$correction" ]; then
    print_green "✅ add.mrl passed!"
else
    print_red "❌ add.mrl failed: Got '$output'"
fi

# mixupadd.mrl
print_bold "Running mixupadd.mrl..."
./glados test/test_files/mixupadd.mrl
output=$(cat out.s)
print_yellow "got '$output'"
correction=$(cat test/test_files/mixupadd.txt)
if [ "$output" == "$correction" ]; then
    print_green "✅ mixupadd.mrl passed!"
else
    print_red "❌ mixupadd.mrl failed: Got '$output'"
fi

# ifbasic.mrl
print_bold "Running ifbasic.mrl..."
./glados test/test_files/ifbasic.mrl
output=$(cat out.s)
print_yellow "got '$output'"
correction=$(cat test/test_files/ifbasic.txt)
if [ "$output" == "$correction" ]; then
    print_green "✅ ifbasic.mrl passed!"
else
    print_red "❌ ifbasic.mrl failed: Got '$output'"
fi

