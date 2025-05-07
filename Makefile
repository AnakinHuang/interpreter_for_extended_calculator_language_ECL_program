#
# CSC254 Assignment 3
#
# Team: Yuesong Huang, Wentao Jiang
# E-mail: yhu116@u.rochecter.edu, wjiang20@u.rochester.edu
#

.PHONY: all clean run

# Compilers and Flags
OCAML = ocamlc
CC = gcc

# Compile rules
all: ecl

ecl:
	$(OCAML) -o ecl str.cma interpreter.ml

ecl_sa:
	$(OCAML) -o ecl_sa str.cma interpreter_static_analysis.ml

# Clean rules
clean:
	rm -f ecl ecl_sa *.cmi *.cmo *_c *.c *.o

# Run rules
run:
	./ecl prog.ecl

run_test1:
	./ecl sum_ave.ecl

run_test2:
	./ecl primes.ecl

run_test3:
	./ecl gcd.ecl

run_test4:
	./ecl sqrt.ecl

run_test5:
	./ecl t2.ecl

run_test6:
	./ecl t3.ecl

run_test7:
	./ecl pow.ecl
	
run_test8:
	./ecl min_max.ecl

run_in:
	./ecl prog.ecl < input

run_test1_in:
	./ecl sum_ave.ecl < input

run_test2_in:
	./ecl primes.ecl < input

run_test3_in:
	./ecl gcd.ecl < input

run_test4_in:
	./ecl sqrt.ecl < input

run_test5_in:
	./ecl t2.ecl < input

run_test6_in:
	./ecl t3.ecl < input

run_test7_in:
	./ecl pow.ecl < input
	
run_test8_in:
	./ecl min_max.ecl < input

# For Extra Credit
run_c_in:
	./ecl prog.ecl < input
	$(CC) -o prog_c prog.c
	./prog_c < input

run_test1_c_in:
	./ecl sum_ave.ecl < input
	$(CC) -o sum_ave_c sum_ave.c
	./sum_ave_c < input

run_test2_c_in:
	./ecl primes.ecl < input
	$(CC) -o primes_c primes.c
	./primes_c < input

run_test3_c_in:
	./ecl gcd.ecl < input
	$(CC) -o gcd_c gcd.c
	./gcd_c < input

run_test4_c_in:
	./ecl sqrt.ecl < input
	$(CC) -o sqrt_c sqrt.c
	./sqrt_c < input

run_test5_c_in:
	./ecl t2.ecl < input
	$(CC) -o t2_c t2.c
	./t2_c < input

run_test6_c_in:
	./ecl t3.ecl < input
	$(CC) -o t3_c t3.c
	./t3_c < input

run_test7_c_in:
	./ecl pow.ecl < input
	$(CC) -o pow_c pow.c
	./pow_c < input

run_test8_c_in:
	./ecl min_max.ecl < input
	$(CC) -o min_max_c min_max.c
	./min_max_c < input

# With Extra Credit Semantic Analysis
run_sa:
	./ecl_sa prog.ecl

run_sa_test1:
	./ecl_sa sum_ave.ecl

run_sa_test2:
	./ecl_sa primes.ecl

run_sa_test3:
	./ecl_sa gcd.ecl

run_sa_test4:
	./ecl_sa sqrt.ecl

run_sa_test5:
	./ecl_sa t2.ecl

run_sa_test6:
	./ecl_sa t3.ecl

run_sa_test7:
	./ecl_sa pow.ecl
	
run_sa_test8:
	./ecl_sa min_max.ecl

run_sa_in:
	./ecl_sa prog.ecl < input

run_sa_test1_in:
	./ecl_sa sum_ave.ecl < input

run_sa_test2_in:
	./ecl_sa primes.ecl < input

run_sa_test3_in:
	./ecl_sa gcd.ecl < input

run_sa_test4_in:
	./ecl_sa sqrt.ecl < input

run_sa_test5_in:
	./ecl_sa t2.ecl < input

run_sa_test6_in:
	./ecl_sa t3.ecl < input

run_sa_test7_in:
	./ecl_sa pow.ecl < input
	
run_sa_test8_in:
	./ecl_sa min_max.ecl < input

# For Extra Credit
run_sa_c_in:
	./ecl_sa prog.ecl < input
	$(CC) -o prog_c prog.c
	./prog_c < input

run_sa_test1_c_in:
	./ecl_sa sum_ave.ecl < input
	$(CC) -o sum_ave_c sum_ave.c
	./sum_ave_c < input

run_sa_test2_c_in:
	./ecl_sa primes.ecl < input
	$(CC) -o primes_c primes.c
	./primes_c < input

run_sa_test3_c_in:
	./ecl_sa gcd.ecl < input
	$(CC) -o gcd_c gcd.c
	./gcd_c < input

run_sa_test4_c_in:
	./ecl_sa sqrt.ecl < input
	$(CC) -o sqrt_c sqrt.c
	./sqrt_c < input

run_sa_test5_c_in:
	./ecl_sa t2.ecl < input
	$(CC) -o t2_c t2.c
	./t2_c < input

run_sa_test6_c_in:
	./ecl_sa t3.ecl < input
	$(CC) -o t3_c t3.c
	./t3_c < input

run_sa_test7_c_in:
	./ecl_sa pow.ecl < input
	$(CC) -o pow_c pow.c
	./pow_c < input

run_sa_test8_c_in:
	./ecl_sa min_max.ecl < input
	$(CC) -o min_max_c min_max.c
	./min_max_c < input
