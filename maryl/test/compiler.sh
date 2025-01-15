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
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == "8" ]; then
    print_green "✅ add.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ add.mrl failed"
fi

# mixupadd.mrl
print_bold "Running mixupadd.mrl..."
./glados test/test_files/mixupadd.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == "6" ]; then
    print_green "✅ mixupadd.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ mixupadd.mrl failed"
fi

# ifbasic.mrl
print_bold "Running ifbasic.mrl..."
./glados test/test_files/ifbasic.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == "2" ]; then
    print_green "✅ ifbasic.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ifbasic.mrl failed"
fi

# elseifelse.mrl
print_bold "Running elseifelse.mrl..."
./glados test/test_files/elseifelse.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == "3" ]; then
    print_green "✅ elseifelse.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ elseifelse.mrl failed"
fi

# basiclist.mrl
print_bold "Running basiclist.mrl..."
./glados test/test_files/basiclist.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == "4" ]; then
    print_green "✅ basiclist.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ basiclist.mrl failed"
fi

# print.mrl
print_bold "Running print.mrl..."
./glados test/test_files/print.mrl
output=$(./glados out.masm --vm | tr -d '\n')
result=$(echo -e "sooo\n1\n5\n['h','e','l','l','o']" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ print.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ print.mrl failed"
fi

# ops.mrl
print_bold "Running ops.mrl..."
./glados test/test_files/ops.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == '1' ]; then
    print_green "✅ ops.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ops.mrl failed"
fi

# ternary.mrl
print_bold "Running ternary.mrl..."
./glados test/test_files/ternary.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == '4' ]; then
    print_green "✅ ternary.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ternary.mrl failed"
fi

# fact.mrl
print_bold "Running fact.mrl..."
./glados test/test_files/fact.mrl
./glados out.masm --vm
output=$(echo $?)
if [ "$output" == '120' ]; then
    print_green "✅ fact.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ fact.mrl failed"
fi
