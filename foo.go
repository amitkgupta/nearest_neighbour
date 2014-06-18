package main

import (
  "io/ioutil"
  "bytes"
  "strconv"
  "fmt"
)

type labelPixels struct {
  Label []byte
  Pixels []uint32
}

var nlbs = []byte("\n")
var cbs = []byte(",")

func slurpFile(filePath string) []labelPixels {
  fileContent, _ := ioutil.ReadFile(filePath)
  thangs := bytes.Split(fileContent,nlbs)
  ln := len(thangs)
  lps := make([]labelPixels, ln-2)
  for i, line := range thangs {
    if i == 0 || i == ln-1 {
      continue
    }

    row := bytes.Split(line, cbs)
    pixels := make([]uint32, len(row)-1)
    for j, k := range row {
      if j == 0 {
        continue
      }

      x, _ := strconv.ParseInt(string(k), 10, 8)
      pixels[j-1] = uint32(x)
    }

    lps[i-1] = labelPixels{row[0], pixels}
  }

  return lps
}

func sqdist(pxs1, pxs2 []uint32) (d uint32) {
  ln := len(pxs1)
  for i := 0; i < ln; i++ {
    x := pxs1[i] - pxs2[i]
    d = d + x*x
  }

  return
}
 
var train = slurpFile("trainingsample.csv")

func classify(pxs []uint32) (lab []byte) {
  lab = train[0].Label
  dist := sqdist(pxs, train[0].Pixels)

  for i, row := range train {
    if i == 0 {
      continue
    }

    ndist := sqdist(pxs, row.Pixels)
    if ndist < dist {
      lab = row.Label
      dist = ndist
    }
  }

  return
}
    
func main() {
  val := slurpFile("validationsample.csv")
  
  tot := 0

  for _, row := range val {
    if string(row.Label) == string(classify(row.Pixels)) {
      tot++
    }
  }

  fmt.Println(tot)
}
