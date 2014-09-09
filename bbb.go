package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math"
	"runtime"
	"strconv"
	"os"
	"log"
	"runtime/pprof"
	"flag"
)

type LabelWithFeatures struct {
	Label    []byte
	Features []float64
}

func NewLabelWithFeatures(parsedLine [][]byte) LabelWithFeatures {
	label := parsedLine[0]
	features := make([]float64, len(parsedLine)-1)

	for i, feature := range parsedLine {
		// skip label
		if i == 0 {
			continue
		}

		features[i-1] = byteSliceTofloat64(feature)
	}

	return LabelWithFeatures{label, features}
}

var newline = []byte("\n")
var comma = []byte(",")

func byteSliceTofloat64(b []byte) float64 {
	x, _ := strconv.ParseFloat(string(b), 32) //10, 8)
	return float64(x)
}

func parseCSVFile(filePath string) []LabelWithFeatures {
	fileContent, _ := ioutil.ReadFile(filePath)
	lines := bytes.Split(fileContent, newline)
	numRows := len(lines)

	labelsWithFeatures := make([]LabelWithFeatures, numRows-2)

	for i, line := range lines {
		// skip headers
		if i == 0 || i == numRows-1 {
			continue
		}

		labelsWithFeatures[i-1] = NewLabelWithFeatures(bytes.Split(line, comma))
	}

	return labelsWithFeatures
}

func squareDistanceWithBailout(features1, features2 []float64, bailout float64) (d float64) {
	for i := 0; i < len(features1); i++ {
		x := features1[i] - features2[i]
		d += x * x

		if d > bailout {
			break
		}
	}

	return
}

var trainingSample = parseCSVFile("many_features_training.csv")

func classify(features []float64) (label []byte) {
	label = trainingSample[0].Label
	d := squareDistanceWithBailout(features, trainingSample[0].Features, math.MaxFloat32)

	for _, row := range trainingSample {
		dNew := squareDistanceWithBailout(features, row.Features, d)

		if dNew < d {
			label = row.Label
			d = dNew
		}
	}

	return
}
var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")


func main() {
flag.Parse()
    if *cpuprofile != "" {
        f, err := os.Create(*cpuprofile)
        if err != nil {
            log.Fatal(err)
        }
        pprof.StartCPUProfile(f)
        defer pprof.StopCPUProfile()
    }
	runtime.GOMAXPROCS(runtime.NumCPU())

println("parsing test")
	validationSample := parseCSVFile("many_features_test.csv")
println("parsed test")

	var totalCorrect float64 = 0
	successChannel := make(chan float64, 10000) //len(validationSample))

	for i := 0; i < 10000; i++ {
		test := validationSample[i]
		go func(t LabelWithFeatures, j int) {
			if j%5 == 0 {
				println("classifying", j)
			}
			if string(t.Label) == string(classify(t.Features)) {
				successChannel <- 1
			} else {
				successChannel <- 0
			}
			if j%5 == 0 {
				println("classified", j)
			}
		}(test, i)
	}

	for i := 0; i < 10000; i++ { //len(validationSample); i++ {
		totalCorrect += <-successChannel
	}

	fmt.Println(float64(totalCorrect)) // / float64(len(validationSample)))
}
