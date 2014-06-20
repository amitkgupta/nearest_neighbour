package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
)

type LabelWithFeatures struct {
	Label    []byte
	Features []uint32
}

func NewLabelWithFeatures(parsedLine [][]byte) LabelWithFeatures {
	label := parsedLine[0]
	features := make([]uint32, len(parsedLine)-1)

	for i, feature := range parsedLine {
		// skip label
		if i == 0 {
			continue
		}

		features[i-1] = byteSliceToUInt32(feature)
	}

	return LabelWithFeatures{label, features}
}

var newline = []byte("\n")
var comma = []byte(",")

func byteSliceToUInt32(b []byte) uint32 {
	x, _ := strconv.ParseInt(string(b), 10, 8)
	return uint32(x)
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

func squareDistanceWithBailout(features1, features2 []uint32, bailout uint32) (d uint32) {
	for i := 0; i < len(features1); i++ {
		x := features1[i] - features2[i]
		d = d + x*x

		if d > bailout {
			break
		}
	}

	return
}

var trainingSample = parseCSVFile("trainingsample.csv")

func classify(features []uint32) (label []byte) {
	label = trainingSample[0].Label
	d := squareDistanceWithBailout(features, trainingSample[0].Features, math.MaxUint32)

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
	validationSample := parseCSVFile("validationsample.csv")

	totalCorrect := 0

	for _, test := range validationSample {
		if string(test.Label) == string(classify(test.Features)) {
			totalCorrect++
		}
	}

	fmt.Println(float64(totalCorrect) / float64(len(validationSample)))
}
