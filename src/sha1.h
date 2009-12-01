/*	$NetBSD: sha1.h,v 1.2 1998/05/29 22:55:44 thorpej Exp $	*/

/*
 * SHA-1 in C
 * By Steve Reid <steve@edmweb.com>
 * 100% Public Domain
 */

#ifndef _sha1_h_
#define	_sha1_h_

typedef struct
{
  u_int32_t state[5];
  u_int32_t count[2];
  u_char buffer[64];
} SHA1_CTX;

void SHA1Init (SHA1_CTX *context);
void SHA1Update (SHA1_CTX *context, const u_char *data, u_int len);
void SHA1Final (u_char digest[20], SHA1_CTX *context);

#endif /* _sha1_h_ */
