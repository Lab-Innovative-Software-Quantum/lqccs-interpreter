PASS_COLOR='\033[1;92m'
FAILED_COLOR='\033[1;91m'
RESET_COLOR='\033[0m'

successTests=
failTests=
flags=

total_test_sources=0
total_pass=0
total_fail=0

function set_flags_of_test_type() {
    case "$1" in
        "parser") 
            flags="-p"
        ;;
        "typechecker") 
            flags="-t"
        ;;
        "interpreter") 
            flags="-i"
        ;;
    esac
}

function setup_global_test_vars() {
    # $1 is the test type, e.g parser or typechecker
    successTests=(`ls ./test/samples/$1/test*.lq`)
    failTests=(`ls ./test/samples/$1/fail*.lq`)
    # retrieve the list of flags for this test type
    set_flags_of_test_type "$1"
}

function run_test() {
    # $1 is the test source file
    { error=$(opam exec -- dune exec bin/lqccsint.exe -- "$flags" "${1}"  2>&1 1>/dev/null); }
    # $2 is the expected result, true for success, false for failure
    local succeded=false
    if [ -z "$error" ]; then 
        succeded=true 
    fi
    
    if [ "$succeded" = "$2" ] # if no error and the expected result is success
    then
        printf "$PASS_COLOR[ PASS ]$RESET_COLOR %s\n" "$1"
        ((total_pass=total_pass+1))
    else
        printf "$FAILED_COLOR[ FAIL ]$RESET_COLOR %s\n%s\n\n" "$1" "$error"
        ((total_fail=total_fail+1))
    fi
}

function run_testsuite() {
    for i in "${!successTests[@]}";
    do
        ((total_test_sources=total_test_sources+1))
        run_test "${successTests[i]}" true
    done

    for i in "${!failTests[@]}";
    do
        ((total_test_sources=total_test_sources+1))
        run_test "${failTests[i]}" false
    done
}

setup_global_test_vars "parser"
run_testsuite

printf "\n\n--------------------------\n"
printf "Total tested sources: %d\n" $total_test_sources
printf "Pass: %d\tFail: %d\n" $total_pass $total_fail
