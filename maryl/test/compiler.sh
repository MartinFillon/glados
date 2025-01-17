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
./glados build test/test_files/add.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == "8" ]; then
    print_green "✅ add.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ add.mrl failed"
fi

# mixupadd.mrl
print_bold "Running mixupadd.mrl..."
./glados build test/test_files/mixupadd.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == "6" ]; then
    print_green "✅ mixupadd.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ mixupadd.mrl failed"
fi

# ifbasic.mrl
print_bold "Running ifbasic.mrl..."
./glados build test/test_files/ifbasic.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == "2" ]; then
    print_green "✅ ifbasic.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ifbasic.mrl failed"
fi

# elseifelse.mrl
print_bold "Running elseifelse.mrl..."
./glados build test/test_files/elseifelse.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == "3" ]; then
    print_green "✅ elseifelse.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ elseifelse.mrl failed"
fi

# basiclist.mrl
print_bold "Running basiclist.mrl..."
./glados build test/test_files/basiclist.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == "4" ]; then
    print_green "✅ basiclist.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ basiclist.mrl failed"
fi

# print.mrl
print_bold "Running print.mrl..."
./glados build test/test_files/print.mrl -o out.masm
output=$(./glados run out.masm | tr -d '\n')
result=$(echo -e "sooo\n1\n5\n['h','e','l','l','o']" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ print.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ print.mrl failed"
fi

# ops.mrl
print_bold "Running ops.mrl..."
./glados build test/test_files/ops.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == '1' ]; then
    print_green "✅ ops.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ops.mrl failed"
fi

# ternary.mrl
print_bold "Running ternary.mrl..."
./glados build test/test_files/ternary.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == '4' ]; then
    print_green "✅ ternary.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ ternary.mrl failed"
fi

# fact.mrl
print_bold "Running fact.mrl..."
./glados build test/test_files/fact.mrl -o out.masm
./glados run out.masm
output=$(echo $?)
if [ "$output" == '120' ]; then
    print_green "✅ fact.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ fact.mrl failed"
fi

# loop.mrl
print_bold "Running loop.mrl..."
./glados build test/test_files/loop.mrl -o out.masm
output=$(./glados run out.masm | tr -d '\n')
result=$(echo -e "5" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ loop.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ loop.mrl failed"
fi

# closeevalerror.mrl
print_bold "Running closeevalerror.mrl..."
./glados build test/test_files/closeevalerror.mrl -o out.masm
output=$(./glados run out.masm | tr -d '\n')
result=$(echo -e "p['h','a','r','r','o']" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ closeevalerror.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ closeevalerror.mrl failed"
fi

# builtin.mrl
print_bold "Running builtin.mrl..."
./glados build test/test_files/builtin.mrl -o out.masm
output=$(./glados run out.masm | tr -d '\n')
result=$(echo -e "hello world" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ builtin.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ builtin.mrl failed"
fi

# basicstruct.mrl
print_bold "Running basicstruct.mrl..."
./glados build test/test_files/basicstruct.mrl -o out.masm
output=$(./glados run out.masm | tr -d '\n')
result=$(echo -e "{"a" = 1,"b" = 2,"c" = 3}{"a" = 0,"b" = 0,"c" = 0}" | tr -d '\n')
if [ "$output" == "$result" ]; then
    print_green "✅ basicstruct.mrl passed!"
else
    print_yellow "got '$output'"
    print_red "❌ basicstruct.mrl failed"
fi

rm out.masm
