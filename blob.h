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
int is_blob(const type* sexp);

/* (make-blob k) procedure */
/* Returns a newly allocated blob of k bytes. The initial */
/* contents of each element is unspecified. */
type* mk_blob(const type* k);

/* (blob-length blob) procedure */
/* Returns the length of blob in bytes as an exact integer. */
type* blob_length(const type* blob);

/* (blob-u8-ref blob k) procedure */
/* Returns the kth byte of blob as an exact integer in the */
/* range [0::255]. */
type* blob_u8_ref(const type* blob, const type* k);

/* (blob-u8-set! blob k u8 ) procedure */
/* Stores u8 as the kth byte of blob. u8 must be an exact */
/* integer in the range [0::255]. The value returned by */
/* blob-u8-set! is unspecifed. */
void blob_u8_set(const type* blob, const type* k, const type* u8);

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


void display_blob(const type* blob, FILE* file);

#endif
