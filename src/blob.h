// MIT license
//
// Copyright 2025 Per Nilsson
///
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
#ifndef _BLOB_H_
#define _BLOB_H_

#include "type.h"

/* Blobs are a disjoint type for representing blocks of binary */
/* data. Conceptually, blobs can be thought of as homogenous */
/* vectors of 8-bit bytes, but they typically occupy less */
/* space than a vector. */
/* The length of a blob is the number of elements that it */
/* contains. This number is a non-negative integer that is */
/* fixed when the blob is created. The valid indexes of a blob */
/* are the exact non-negative integers less than the length of */
/* the blob, starting at index zero as with vectors. */

/* (blob? obj ) procedure */
/* Returns #t iff obj is a blob. */
int is_blob(const TYPE* sexp);

/* (make-blob k) procedure */
/* Returns a newly allocated blob of k bytes. The initial */
/* contents of each element is unspecified. */
TYPE* mk_blob(const TYPE* k);

/* (blob-length blob) procedure */
/* Returns the length of blob in bytes as an exact integer. */
TYPE* blob_length(const TYPE* blob);

/* (blob-u8-ref blob k) procedure */
/* Returns the kth byte of blob as an exact integer in the */
/* range [0::255]. */
TYPE* blob_u8_ref(const TYPE* blob, const TYPE* k);

/* (blob-u8-set! blob k u8 ) procedure */
/* Stores u8 as the kth byte of blob. u8 must be an exact */
/* integer in the range [0::255]. The value returned by */
/* blob-u8-set! is unspecifed. */
void blob_u8_set(const TYPE* blob, const TYPE* k, const TYPE* u8);

/* (blob-copy blob) procedure */
/* Returns a newly allocated blob containing the same bytes */
/* as blob. */

/* (blob-copy! from to) procedure */
/* Copy the bytes of blob from to blob to, which must not be */
/* shorter. The value returned by copy-blob! is unspecified. */
/* (partial-blob blob start end) procedure */
/* Returns a newly allocated blob containing the bytes in blob */
/* between start (inclusive) and end (exclusive). */

/* (partial-blob-copy! from start end to at) procedure */
/* Copy the bytes of blob from between start and end to blob */
/* to, starting at at. The order in which bytes are copied */
/* is unspecified, except that if the source and destination */
/* overlap, copying takes place as if the source is first copied */
/* into a temporary blob and then into the destination. This */
/* can be achieved without allocating storage by making sure */
/* to copy in the correct direction in such circumstances. */


/* The inequality (>= (- (blob-length to) at) (- end */
/* start)) must be true. The value returned by copy-blob! */
/* is unspecified. */


void display_blob(const TYPE* blob, FILE* file);

#endif
