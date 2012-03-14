/*
 * Process Spawn Manager
 *
 * Copyright (C) 2012 Matthias Weidle <matthias.weidle@secunet.com>
 * Copyright (C) 2012 secunet Security Networks AG
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 * As a special exception, if other files instantiate generics from this
 * unit,  or  you  link  this  unit  with  other  files  to  produce  an
 * executable   this  unit  does  not  by  itself  cause  the  resulting
 * executable to  be  covered by the  GNU General  Public License.  This
 * exception does  not  however  invalidate  any  other reasons why  the
 * executable file might be covered by the GNU Public License.
 */

#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/*
 * Call execv with an empty signal mask so that no signals are blocked per
 * default
 */
int main(int argc, char *argv[])
{
	sigset_t mask;

	if (argc < 2)
	{
		fprintf(stderr, "Need at least one parameter\n");
		exit(1);
	}

	sigemptyset(&mask);
	if (sigprocmask(SIG_SETMASK, &mask, NULL) < 0)
	{
		perror("sigprocmask");
		exit(1);
	}

	argv++;
	execv(argv[0], argv);
	perror("exec");
	return 1;
}
