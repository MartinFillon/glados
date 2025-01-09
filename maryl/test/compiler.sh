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
output=$(./glados out.s --vm)
if [ "$output" == "8" ]; then
    print_green "✅ add.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ add.mrl failed"
fi

# mixupadd.mrl
print_bold "Running mixupadd.mrl..."
./glados test/test_files/mixupadd.mrl
output=$(./glados out.s --vm)
if [ "$output" == "6" ]; then
    print_green "✅ mixupadd.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ mixupadd.mrl failed"
fi

# ifbasic.mrl
print_bold "Running ifbasic.mrl..."
./glados test/test_files/ifbasic.mrl
output=$(./glados out.s --vm)
if [ "$output" == "2" ]; then
    print_green "✅ ifbasic.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ifbasic.mrl failed"
fi

# elseifelse.mrl
print_bold "Running elseifelse.mrl..."
./glados test/test_files/elseifelse.mrl
output=$(./glados out.s --vm)
if [ "$output" == "3" ]; then
    print_green "✅ elseifelse.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ elseifelse.mrl failed"
fi

# basiclist.mrl
print_bold "Running basiclist.mrl..."
./glados test/test_files/basiclist.mrl
output=$(./glados out.s --vm)
if [ "$output" == "4" ]; then
    print_green "✅ basiclist.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ basiclist.mrl failed"
fi