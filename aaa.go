package main

import (
	"runtime"
	"fmt"
	"github.com/amitkgupta/goodlearn/csvparse"
	"github.com/amitkgupta/goodlearn/classifier/knn"
	"github.com/amitkgupta/goodlearn/data/row"

	"os"
	"log"
	"flag"
	"runtime/pprof"
)
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
	validationSample, _ := csvparse.DatasetFromPath("many_features_test.csv", 0, 1)
println("parsed test")
println("parsing training")
	trainingSample, _ := csvparse.DatasetFromPath("many_features_training.csv", 0, 1)
println("parsed training")

c, _ := knn.NewKNNClassifier(1)
c.Train(trainingSample)

	var totalCorrect float32 = 0
	successChannel := make(chan float32, 10000) //len(validationSample))

	for i := 0; i < 10000; i++ {
		test, _ := validationSample.Row(i)
		go func(t row.Row, j int) {
			if j%5 == 0 {
				println("classifying", j)
			}
cl, _ := c.Classify(test)		
if cl.Equals(test.Target()) {
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

	fmt.Println(float32(totalCorrect)) // / float32(len(validationSample)))
}
