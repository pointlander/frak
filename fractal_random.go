package frak

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/png"
	"io"
	"log"
	"math"
	"math/rand"
	"os"

	"github.com/nfnt/resize"
	"github.com/pointlander/gobrain"
)

func FractalCoderRandom(in image.Image, panelSize int, out io.Writer) {
	destination := newImage(in, 1, panelSize, 1)
	reference := newImage(in, 2, panelSize, GAMMA)
	maps := newPixelMapIdentity(panelSize)
	forms := make([]int, len(maps))

	formsBuffer, betaBuffer, count := &bytes.Buffer{}, &bytes.Buffer{}, 0
	write8 := func(i byte) {
		b := [...]byte{i}
		formsBuffer.Write(b[:])
	}
	writeBeta := func(i byte) {
		b := [...]byte{i}
		betaBuffer.Write(b[:])
	}

	bestSeed, bestTotalError := 0, uint64(math.MaxUint64)
	var bestFormsBuffer, bestBetaBuffer *bytes.Buffer
	for c := 0; c < 100; c++ {
		rnd, totalError := rand.New(rand.NewSource(int64(c))), uint64(0)
		for _, dPanel := range destination.panels {
			bestError, bestForm, bestBeta := uint64(math.MaxUint64), 0, 0
			rPanel := reference.panels[rnd.Intn(len(reference.panels))]
			beta := dPanel.mean - rPanel.mean
		search:
			for f, pmap := range maps {
				error := uint64(0)
				for i, j := range pmap {
					delta := int(dPanel.pixels[i]) -
						int(rPanel.pixels[j]) -
						beta
					error += uint64(delta * delta)
					if error >= bestError {
						continue search
					}
				}
				if error < bestError {
					bestBeta = beta
					bestForm = f
					bestError = error
				}
				if bestError == 0 {
					break
				}
			}
			write8(uint8(bestForm))
			writeBeta(uint8(int8(bestBeta>>1) + 127))
			totalError += bestError
		}
		if totalError < bestTotalError {
			bestTotalError, bestSeed, bestFormsBuffer, bestBetaBuffer = totalError, c, formsBuffer, betaBuffer
			fmt.Println(bestTotalError)
		}
		formsBuffer, betaBuffer = &bytes.Buffer{}, &bytes.Buffer{}
	}

	net := &gobrain.FeedForward32{}
	width := panelSize * panelSize
	net.Init(width, width/2, width)
	patterns := make([][][]float32, len(destination.panels))
	rnd := rand.New(rand.NewSource(int64(bestSeed)))
	for i, dPanel := range destination.panels {
		rPanel := reference.panels[rnd.Intn(len(reference.panels))]
		inputs := make([]float32, len(rPanel.pixels))
		for j, pixel := range rPanel.pixels {
			inputs[j] = float32(pixel) / 255
		}

		outputs := make([]float32, len(dPanel.pixels))
		for j, pixel := range dPanel.pixels {
			outputs[j] = float32(pixel) / 255
		}

		patterns[i] = [][]float32{inputs, outputs}
	}
	net.Train(patterns, 10, 0.6, 0.4, false)

	rnd = rand.New(rand.NewSource(int64(bestSeed)))
	netError := 0
	for _, dPanel := range destination.panels {
		rPanel := reference.panels[rnd.Intn(len(reference.panels))]
		inputs := make([]float32, len(rPanel.pixels))
		for i, pixel := range rPanel.pixels {
			inputs[i] = float32(pixel) / 256
		}

		outputs := net.Update(inputs)
		mean := 0
		for _, output := range outputs {
			mean += int(output * 255)
		}
		beta := dPanel.mean - (mean / len(outputs))

		for i, output := range outputs {
			delta := int(dPanel.pixels[i]) -
				int(output*255) -
				beta
			netError += int(delta * delta)
		}
	}
	fmt.Println(netError)

	for _, form := range bestFormsBuffer.Bytes() {
		forms[form]++
		count++
	}

	write32 := func(i uint32) {
		b := [...]byte{
			byte(i >> 24),
			byte((i >> 16) & 0xFF),
			byte((i >> 8) & 0xFF),
			byte(i & 0xFF)}
		out.Write(b[:])
	}
	writeBlock := func(buffer *bytes.Buffer) {
		output := press(buffer)
		write32(uint32(output.Len()))
		out.Write(output.Bytes())
		write32(uint32(buffer.Len()))
	}
	write32(uint32(destination.xPanels))
	write32(uint32(destination.yPanels))
	write32(uint32(destination.panelSize))
	write32(uint32(count))
	write32(uint32(bestSeed))
	writeBlock(bestFormsBuffer)
	writeBlock(bestBetaBuffer)
}

func FractalDecoderRandom(in io.Reader, _panelSize int) image.Image {
	read32 := func() uint32 {
		var p [4]byte
		in.Read(p[:])
		return (uint32(p[0]) << 24) |
			(uint32(p[1]) << 16) |
			(uint32(p[2]) << 8) |
			uint32(p[3])
	}
	xPanels := read32()
	yPanels := read32()
	panelSize := read32()
	count := read32()
	seed := read32()

	readBlock := func() *bytes.Buffer {
		length := int(read32())
		buffer := make([]byte, length)
		n, err := in.Read(buffer)
		if err != nil {
			log.Fatal(err)
		}
		if n != length {
			log.Fatal("not all bytes read")
		}
		length = int(read32())
		return unpress(bytes.NewBuffer(buffer), length)
	}
	formsBuffer := readBlock()
	betaBuffer := readBlock()

	codes := make([]struct {
		form, x, y uint16
		beta       int
	}, count)

	read8 := func() uint8 {
		var p [1]byte
		formsBuffer.Read(p[:])
		return p[0]
	}
	i := 0
	forms := image.NewGray(image.Rect(0, 0, int(xPanels), int(yPanels)))
	for y := 0; y < int(yPanels); y++ {
		for x := 0; x < int(xPanels); x++ {
			form := read8()
			codes[i].form = uint16(form)
			forms.SetGray(x, y, color.Gray{Y: form << 5})
			i++
		}
	}
	file, err := os.Create("forms.png")
	if err != nil {
		log.Fatal(err)
	}

	err = png.Encode(file, forms)
	if err != nil {
		log.Fatal(err)
	}

	read8 = func() uint8 {
		var p [1]byte
		betaBuffer.Read(p[:])
		return p[0]
	}
	jpg := image.NewGray(image.Rect(0, 0, int(xPanels), int(yPanels)))
	for y := 0; y < int(yPanels); y++ {
		for x := 0; x < int(xPanels); x++ {
			jpg.SetGray(x, y, color.Gray{Y: read8()})
		}
	}
	//jpg = IPaeth8(jpg)
	i = 0
	for y := 0; y < int(yPanels); y++ {
		for x := 0; x < int(xPanels); x++ {
			g, _, _, _ := jpg.At(x, y).RGBA()
			codes[i].beta = (int(g>>8) - 127) << 1
			i++
		}
	}

	width, height := xPanels*uint32(_panelSize), yPanels*uint32(_panelSize)
	pixels := make([]uint8, width*height)
	for y := uint32(0); y < height; y++ {
		offset := y * width
		for x := uint32(0); x < width; x++ {
			pixels[offset+x] = uint8(0x80)
		}
	}

	panels, size := make([]imagePanel, xPanels*yPanels), _panelSize*_panelSize
	for i := range panels {
		panels[i].pixels = make([]uint16, size)
	}

	destination := &image8{
		xPanels:   int(xPanels),
		yPanels:   int(yPanels),
		panelSize: _panelSize,
		bounds: image.Rectangle{
			Max: image.Point{
				X: int(width),
				Y: int(height)}},
		pixels: pixels,
		panels: panels}
	destination.updatePanels()

	newReference := func() *image8 {
		width, height := destination.Bounds().Max.X, destination.Bounds().Max.Y
		width, height = width/2, height/2
		reference := resize.Resize(uint(width), uint(height), destination, resize.NearestNeighbor)

		pixels := make([]uint8, width*height)
		for y := 0; y < height; y++ {
			offset := y * width
			for x := 0; x < width; x++ {
				r, _, _, _ := reference.At(x, y).RGBA()
				pixels[offset+x] = uint8(uint32(round(float64(r) * GAMMA / 256)))
			}
		}

		xPanels, yPanels := width/_panelSize, height/_panelSize
		panels, size := make([]imagePanel, xPanels*yPanels), _panelSize*_panelSize
		for i := range panels {
			panels[i].pixels = make([]uint16, size)
		}

		paneled := &image8{
			xPanels:   xPanels,
			yPanels:   yPanels,
			panelSize: _panelSize,
			bounds: image.Rectangle{
				Max: image.Point{
					X: width,
					Y: height}},
			pixels: pixels,
			panels: panels}
		paneled.updatePanels()
		return paneled
	}

	maps := newPixelMap(_panelSize)
	reference := newReference()
	rnd := rand.New(rand.NewSource(int64(seed)))
	for i := range codes {
		rPanel := reference.panels[rnd.Intn(len(reference.panels))]
		codes[i].x = uint16(rPanel.x)
		codes[i].y = uint16(rPanel.y)
	}
	for i := 0; i < DECODE_ITERATIONS; i++ {
		for j, d := range panels {
			code := codes[j]
			//x, y := int(uint64(_panelSize)*uint64(code.x)/(2*uint64(panelSize))),
			//	int(uint64(_panelSize)*uint64(code.y)/(2*uint64(panelSize)))
			x, y := int(uint32(code.x)/panelSize),
				int(uint32(code.y)/panelSize)
			if x >= reference.xPanels {
				x = reference.xPanels - 1
			}
			if y >= reference.yPanels {
				y = reference.yPanels - 1
			}
			r := reference.panels[x+y*reference.xPanels]
			pmap, f := maps[code.form], 0

			for y := 0; y < _panelSize; y++ {
				for x := 0; x < _panelSize; x++ {
					z, e := int(r.pixels[pmap[f]])+code.beta,
						d.x+x+int(width)*(d.y+y)
					if z < 0 {
						pixels[e] = uint8(0)
					} else if z > 255 {
						pixels[e] = uint8(0xFF)
					} else {
						pixels[e] = uint8(z)
					}
					f++
				}
			}
		}
		reference = newReference()
	}

	return destination
}

func FractalCoderRandomMark3(in image.Image, panelSize int, out io.Writer) {
	destination := newImage(in, 1, panelSize, 1)
	reference := newImage(in, 2, panelSize, GAMMA)

	betaBuffer, count := &bytes.Buffer{}, 0
	writeBeta := func(i byte) {
		b := [...]byte{i}
		betaBuffer.Write(b[:])
	}

	width := panelSize * panelSize
	bestSeed, bestTotalError := 0, uint64(math.MaxUint64)
	var bestBetaBuffer *bytes.Buffer
	var bestNet *gobrain.FeedForward32
	for c := 0; c < 100; c++ {
		rnd, totalError := rand.New(rand.NewSource(int64(c))), uint64(0)
		net := &gobrain.FeedForward32{
			Dropout: .5,
		}
		net.Init(width, width/2, width)
		patterns := make([][][]float32, len(destination.panels))
		for i, dPanel := range destination.panels {
			rPanel := reference.panels[rnd.Intn(len(reference.panels))]
			inputs := make([]float32, len(rPanel.pixels))
			for j, pixel := range rPanel.pixels {
				inputs[j] = float32(pixel) / 255
			}

			outputs := make([]float32, len(dPanel.pixels))
			for j, pixel := range dPanel.pixels {
				outputs[j] = float32(pixel) / 255
			}

			patterns[i] = [][]float32{inputs, outputs}
		}
		net.Train(patterns, 1, 0.6, 0.4, false)

		rnd = rand.New(rand.NewSource(int64(c)))
		for _, dPanel := range destination.panels {
			rPanel := reference.panels[rnd.Intn(len(reference.panels))]
			inputs := make([]float32, len(rPanel.pixels))
			for i, pixel := range rPanel.pixels {
				inputs[i] = float32(pixel) / 256
			}

			outputs := net.Update(inputs)
			mean := 0
			for _, output := range outputs {
				mean += int(output * 255)
			}
			beta := dPanel.mean - (mean / len(outputs))

			for i, output := range outputs {
				delta := int(dPanel.pixels[i]) -
					int(output*255) -
					beta
				totalError += uint64(delta * delta)
			}

			writeBeta(uint8(int8(beta>>1) + 127))
		}

		if totalError < bestTotalError {
			bestTotalError, bestSeed, bestBetaBuffer, bestNet = totalError, c, betaBuffer, net
			fmt.Println(bestTotalError)
		}
		betaBuffer = &bytes.Buffer{}
	}

	_ = bestNet

	for range bestBetaBuffer.Bytes() {
		count++
	}

	write32 := func(i uint32) {
		b := [...]byte{
			byte(i >> 24),
			byte((i >> 16) & 0xFF),
			byte((i >> 8) & 0xFF),
			byte(i & 0xFF)}
		out.Write(b[:])
	}
	writeBlock := func(buffer *bytes.Buffer) {
		output := press(buffer)
		write32(uint32(output.Len()))
		out.Write(output.Bytes())
		write32(uint32(buffer.Len()))
	}
	write32(uint32(destination.xPanels))
	write32(uint32(destination.yPanels))
	write32(uint32(destination.panelSize))
	write32(uint32(count))
	write32(uint32(bestSeed))
	writeBlock(bestBetaBuffer)
}
