package embed

import "io"

type ReadWriter struct {
	reader *io.Reader
	writer *io.Writer
}
