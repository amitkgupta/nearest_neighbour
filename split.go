package main

import (
	"os"
	"fmt"
	"math/rand"
	"bufio"
)

func thang(basename string, extension string, trainTestSplit float64) {
	file, err := os.Open(fmt.Sprintf("%s.%s", basename, extension))
	if err != nil {
   		panic(err)
	}
	defer file.Close()

	train, err := os.Create(fmt.Sprintf("%s_train.%s", basename, extension))
	if err != nil {
		panic(err)	
	}
	defer train.Close()

	test, err := os.Create(fmt.Sprintf("%s_test.%s", basename, extension))
	if err != nil {
		panic(err)	
	}
	defer test.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	test.WriteString(fmt.Sprintf("%s\n", scanner.Text()))
	train.WriteString(fmt.Sprintf("%s\n", scanner.Text()))

	for scanner.Scan() {
		if rand.Float64() < trainTestSplit {
			train.WriteString(fmt.Sprintf("%s\n", scanner.Text()))
		} else {
			test.WriteString(fmt.Sprintf("%s\n", scanner.Text()))
		}
	}

	if err := scanner.Err(); err != nil {
    		panic(err)
	}
}

func main() {
   thang("many_features", "csv", 0.6)
}
