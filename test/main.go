package main

import (
	"bytes"
	"fmt"
	"image"
	"image/jpeg"
	"image/png"
	"log"
	"os"
	"strings"

	"github.com/nfnt/resize"
	"github.com/pointlander/frak"
)

const (
	testImage = "../images/image01.png"
)

func main() {
	fcpBuffer := &bytes.Buffer{}
	file, err := os.Open(testImage)
	if err != nil {
		log.Fatal(err)
	}

	info, err := file.Stat()
	if err != nil {
		log.Fatal(err)
	}
	name := info.Name()
	name = name[:strings.Index(name, ".")]

	input, _, err := image.Decode(file)
	if err != nil {
		log.Fatal(err)
	}
	file.Close()

	width, height, scale := input.Bounds().Max.X, input.Bounds().Max.Y, 1
	width, height = width/scale, height/scale
	width, height = 512, 512
	input = resize.Resize(uint(width), uint(height), input, resize.NearestNeighbor)

	gray := frak.Gray(input)
	frak.FractalCoderRandomMark3(gray, 4, fcpBuffer)
	length := fcpBuffer.Len()

	file, err = os.Create(name + ".png")
	if err != nil {
		log.Fatal(err)
	}

	err = png.Encode(file, input)
	if err != nil {
		log.Fatal(err)
	}
	file.Close()

	file, err = os.Create(name + ".fcp")
	if err != nil {
		log.Fatal(err)
	}
	file.Write(fcpBuffer.Bytes())
	file.Close()

	/*decoded := frak.FractalDecoderNext(fcpBuffer, 4)

	file, err = os.Create(name + "_decoded.png")
	if err != nil {
		log.Fatal(err)
	}

	err = png.Encode(file, decoded)
	if err != nil {
		log.Fatal(err)
	}
	file.Close()*/

	compress_test := func() (int, string) {
		return length, "github.com/pointlander/compress"
	}

	jpg_test := func() (int, string) {
		buffer := &bytes.Buffer{}
		jpeg.Encode(buffer, gray, nil)
		return buffer.Len(), "image/jpeg"
	}

	tests := []func() (int, string){compress_test, jpg_test}
	results := make([]struct {
		size int
		name string
	}, len(tests))
	for i, test := range tests {
		results[i].size, results[i].name = test()
		if i < 5 {
			fmt.Printf("%.3f%% %7vb %v\n", 100*float64(results[i].size)/float64(length), results[i].size, results[i].name)
		}
	}

	fmt.Println("\n image compression comparisons")
	for _, result := range results {
		fmt.Printf("%.3f%% %7vb %v\n", 100*float64(result.size)/float64(width*height), result.size, result.name)
	}
}
