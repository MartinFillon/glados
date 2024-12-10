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

# foo.scm
print_bold "Running foo.scm..."
output=$(./glados < test/test_files/foo.scm)
print_yellow "got '$output'"
if [ "$output" == "42" ]; then
    print_green "✅ foo.scm passed!"
else
    print_red "❌ foo.scm failed: Got '$output'"
fi

# error.scm
print_bold "Running error.scm..."
output=$(./glados < test/test_files/error.scm)
return=$?
if [ $return -eq 84 ]; then 
    print_green "✅ error.scm passed! returned '$return'"
else
    print_red "❌ error.scm failed: returned '$return'"
fi

# call.scm
print_bold "Running call.scm..."
output=$(./glados < test/test_files/call.scm)
print_yellow "got '$output'"
if [ "$output" == "5" ]; then
    print_green "✅ call.scm passed!"
else
    print_red "❌ call.scm failed: Got '$output'"
fi

# lambda1.scm
print_bold "Running lambda1.scm..."
output=$(./glados < test/test_files/lambda1.scm)
return=$?
print_yellow "got '$output'"
if [ $return -eq 0 ]; then 
    print_green "✅ lambda1.scm passed!" 
else
    print_red "❌ lambda1.scm failed: returned '$return'"
fi

# lambda2.scm
print_bold "Running lambda2.scm..."
output=$(./glados < test/test_files/lambda2.scm)
print_yellow "got '$output'"
if [ "$output" == "3" ]; then
    print_green "✅ lambda2.scm passed!"
else
    print_red "❌ lambda2.scm failed: Got '$output'"
fi

# lambda3.scm
print_bold "Running lambda3.scm..."
output=$(./glados < test/test_files/lambda3.scm)
print_yellow "got '$output'"
if [ "$output" == "7" ]; then
    print_green "✅ lambda3.scm passed!"
else
    print_red "❌ lambda3.scm failed: Got '$output'"
fi

# function1.scm
print_bold "Running function1.scm..."
output=$(./glados < test/test_files/function1.scm)
print_yellow "got '$output'"
if [ "$output" == "7" ]; then
    print_green "✅ function1.scm passed!"
else
    print_red "❌ function1.scm failed: Got '$output'"
fi

# if1.scm
print_bold "Running if1.scm..."
output=$(./glados < test/test_files/if1.scm)
print_yellow "got '$output'"
if [ "$output" == "1" ]; then
    print_green "✅ if1.scm passed!"
else
    print_red "❌ if1.scm failed: Got '$output'"
fi

# if2.scm
print_bold "Running if2.scm..."
output=$(./glados < test/test_files/if2.scm)
print_yellow "got '$output'"
if [ "$output" == "2" ]; then
    print_green "✅ if2.scm passed!"
else
    print_red "❌ if2.scm failed: Got '$output'"
fi

# if3.scm
print_bold "Running if3.scm..."
output=$(./glados < test/test_files/if3.scm)
print_yellow "got '$output'"
if [ "$output" == "21" ]; then
    print_green "✅ if3.scm passed!"
else
    print_red "❌ if3.scm failed: Got '$output'"
fi

# builtins1.scm
print_bold "Running builtins1.scm..."
output=$(./glados < test/test_files/builtins1.scm)
print_yellow "got '$output'"
if [ "$output" == "11" ]; then
    print_green "✅ builtins1.scm passed!"
else
    print_red "❌ builtins1.scm failed: Got '$output'"
fi

# builtins2.scm
print_bold "Running builtins2.scm..."
output=$(./glados < test/test_files/builtins2.scm)
print_yellow "got '$output'"
if [ "$output" == "#t" ]; then
    print_green "✅ builtins2.scm passed!"
else
    print_red "❌ builtins2.scm failed: Got '$output'"
fi

# builtins3.scm
print_bold "Running builtins3.scm..."
output=$(./glados < test/test_files/builtins3.scm)
print_yellow "got '$output'"
if [ "$output" == "#f" ]; then
    print_green "✅ builtins3.scm passed!"
else
    print_red "❌ builtins3.scm failed: Got '$output'"
fi

# superior.scm
print_bold "Running superior.scm..."
output=$(./glados < test/test_files/superior.scm)
print_yellow "got '$output'"
if [ "$output" == "#t" ]; then
    print_green "✅ superior.scm passed!"
else
    print_red "❌ superior.scm failed: Got '$output'"
fi

# # factorial.scm
# echo "Running factorial.scm..."
# output=$(./glados < test/test_files/factorial.scm)
# print_yellow "got '$output'"
# if [ "$output" == "3628800" ]; then
#     print_green "✅ factorial.scm passed!"
# else
#     print_red "❌ factorial.scm failed: Got '$output'"
# fi
