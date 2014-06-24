package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math"
	"runtime"
	"strconv"
)

type LabelWithFeatures struct {
	Label    []byte
	Features []float32
}

func NewLabelWithFeatures(parsedLine [][]byte) LabelWithFeatures {
	label := parsedLine[0]
	features := make([]float32, len(parsedLine)-1)

	for i, feature := range parsedLine {
		// skip label
		if i == 0 {
			continue
		}

		features[i-1] = byteSliceTofloat32(feature)
	}

	return LabelWithFeatures{label, features}
}

var newline = []byte("\n")
var comma = []byte(",")

func byteSliceTofloat32(b []byte) float32 {
	x, _ := strconv.ParseFloat(string(b), 32) //10, 8)
	return float32(x)
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

func squareDistanceWithBailout(features1, features2 []float32, bailout float32) (d float32) {
	for i := 0; i < len(features1); i++ {
		x := features1[i] - features2[i]
		d += x * x

		if d > bailout {
			break
		}
	}

	return
}

var trainingSample = parseCSVFile("trainingsample.csv")

func classify(features []float32) (label []byte) {
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

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())

	validationSample := parseCSVFile("validationsample.csv")

	var totalCorrect float32 = 0
	successChannel := make(chan float32)

	for _, test := range validationSample {
		go func(t LabelWithFeatures) {
			if string(t.Label) == string(classify(t.Features)) {
				successChannel <- 1
			} else {
				successChannel <- 0
			}
		}(test)
	}

	for i := 0; i < len(validationSample); i++ {
		totalCorrect += <-successChannel
	}

	fmt.Println(float32(totalCorrect) / float32(len(validationSample)))
}
