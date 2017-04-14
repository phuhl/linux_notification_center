# linux-notification-center - A notification center and notification daemon
# See LICENSE file for copyright and license details.

PREFIX ?= /usr
MANPREFIX = ${PREFIX}/share/man

all: stack service


stack: 
	stack setup
	stack install --local-bin-path .out

clean-stack:
	rm -f -r .stack-work
	rm -f -r .out
	rm -f com.ph-uhl.deadd.notification.service
	rm -f deadd.notification.systemd.service

clean-doc:
#	rm -f docs/dunst.1

clean: clean-stack clean-doc

#distclean: clean clean-config

#clean-config:
#	rm -f config.h

doc:
	stack haddock

service:
	@sed "s|##PREFIX##|$(PREFIX)|" com.ph-uhl.deadd.notification.service.in > com.ph-uhl.deadd.notification.service
	@sed "s|##PREFIX##|$(PREFIX)|" deadd.notification.systemd.service.in > deadd.notification.systemd.service

install-stack:
	mkdir -p ${DESTDIR}${PREFIX}/bin
	install -m755 .out/linux-notification-center ${DESTDIR}${PREFIX}/bin
	mkdir -p ${DESTDIR}${MANPREFIX}/man1
	install -m644 docs/linux-notification-center.man ${DESTDIR}${MANPREFIX}/man1/linux-notification-center.1

#install-doc:
#	mkdir -p ${DESTDIR}${PREFIX}/share/dunst
#	install -m644 dunstrc ${DESTDIR}${PREFIX}/share/dunst

install-service: service
	mkdir -p ${DESTDIR}${PREFIX}/share/dbus-1/services/
	install -m644 com.ph-uhl.deadd.notification.service ${DESTDIR}${PREFIX}/share/dbus-1/services

install: install-stack install-service

uninstall:
	rm -f ${DESTDIR}${PREFIX}/bin/linux-notification-center
	rm -f ${DESTDIR}${MANPREFIX}/man1/linux-notification-center.1
	rm -f ${DESTDIR}${PREFIX}/share/dbus-1/services/com.ph-uhl.deadd.notification.service

.PHONY: all clean install uninstall
