package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strconv"
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
	x, _ := strconv.ParseFloat(string(b), 32)
	return x
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

func squareDistance(features1, features2 []float64) (d float64) {  
	for i := 0; i < len(features1); i++ {
    d += (features1[i] - features2[i]) * (features1[i] - features2[i])
  }

  return
}

var trainingSample = parseCSVFile("trainingsample.csv")

func classify(features []float64) (label []byte) {
	label = trainingSample[0].Label
	d := squareDistance(features, trainingSample[0].Features)

	for _, row := range trainingSample {
		dNew := squareDistance(features, row.Features)
		if dNew < d {
			label = row.Label
			d = dNew
		}
	}

	return
}

func main() {
	validationSample := parseCSVFile("validationsample.csv")

	totalCorrect := 0

	for _, test := range validationSample {
		if string(test.Label) == string(classify(test.Features)) {
			totalCorrect++
		}
	}

	fmt.Println(float64(totalCorrect) / float64(len(validationSample)))
}
