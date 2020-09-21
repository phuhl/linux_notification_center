# deadd-notification-center - A notification center and notification daemon
# See LICENSE file for copyright and license details.

PREFIX ?= /usr
MANPREFIX = ${PREFIX}/share/man
PKG_CONFIG ?= pkg-config
SYSTEMCTL ?= systemctl

ifeq (,${SYSTEMD})
# Check for systemctl to avoid discrepancies on systems, where
# systemd is installed, but systemd.pc is in another package
systemctl := $(shell command -v ${SYSTEMCTL} >/dev/null && echo systemctl)
ifeq (systemctl,${systemctl})
SYSTEMD := 1
else
SYSTEMD := 0
endif
endif

ifneq (0,${SYSTEMD})
SERVICEDIR_SYSTEMD ?= $(shell $(PKG_CONFIG) systemd --variable=systemduserunitdir)
SERVICEDIR_SYSTEMD := ${SERVICEDIR_SYSTEMD}
ifeq (,${SERVICEDIR_SYSTEMD})
$(error "Failed to query $(PKG_CONFIG) for package 'systemd'!")
endif
endif

SERVICEDIR_DBUS ?= $(shell $(PKG_CONFIG) dbus-1 --variable=session_bus_services_dir)
SERVICEDIR_DBUS := ${SERVICEDIR_DBUS}



all: stack service


stack: 
	stack setup
	stack install --local-bin-path .out

clean-stack:
	rm -f -r .stack-work
	rm -f -r .out
	rm -f com.ph-uhl.deadd.notification.service
	rm -f deadd.notification.systemd.service

clean: clean-stack

distclean: clean clean-config

clean-config:
	rm -f ${HOME}/.config/deadd/deadd.conf

doc:
	stack haddock

service:
	@sed "s|##PREFIX##|$(PREFIX)|" com.ph-uhl.deadd.notification.service.in > com.ph-uhl.deadd.notification.service
	@sed "s|##PREFIX##|$(PREFIX)|" deadd-notification-center.service.in > deadd-notification-center.service

install-stack:
	mkdir -p ${DESTDIR}${PREFIX}/bin
	install -Dm755 .out/deadd-notification-center ${DESTDIR}${PREFIX}/bin
	mkdir -p ${DESTDIR}${MANPREFIX}/man1
	install -Dm644 docs/linux-notification-center.man ${DESTDIR}${MANPREFIX}/man1/deadd-notification-center.1
	install -Dm644 LICENSE ${DESTDIR}${PREFIX}/share/licenses/deadd-notification-center/LICENSE

install-service: service
	mkdir -p ${DESTDIR}${SERVICEDIR_DBUS}
	install -Dm644 com.ph-uhl.deadd.notification.service ${DESTDIR}${SERVICEDIR_DBUS}
ifneq (0,${SYSTEMD})
install-service: install-service-systemd
install-service-systemd:
	mkdir -p ${DESTDIR}${SERVICEDIR_SYSTEMD}
	install -Dm644 deadd-notification-center.service ${DESTDIR}${SERVICEDIR_SYSTEMD}
endif




install-lang:
	mkdir -p ${DESTDIR}${PREFIX}/share/locale/de/LC_MESSAGES
	mkdir -p ${DESTDIR}${PREFIX}/share/locale/en/LC_MESSAGES
	install -Dm644 translation/de/LC_MESSAGES/deadd-notification-center.mo ${DESTDIR}${PREFIX}/share/locale/de/LC_MESSAGES/deadd-notification-center.mo
	install -Dm644 translation/en/LC_MESSAGES/deadd-notification-center.mo ${DESTDIR}${PREFIX}/share/locale/en/LC_MESSAGES/deadd-notification-center.mo

install: install-stack install-service install-lang

uninstall:
	rm -f ${DESTDIR}${PREFIX}/bin/deadd-notification-center
	rm -f ${DESTDIR}${MANPREFIX}/man1/deadd-notification-center.1
	rm -f ${DESTDIR}${SERVICEDIR_DBUS}/com.ph-uhl.deadd.notification.service
	rm -f ${DESTDIR}${PREFIX}/share/licenses/deadd-notification-center/LICENSE
	rm -f ${DESTDIR}${PREFIX}/share/locale/{de,en}/LC_MESSAGES/deadd-notification-center.mo

ifneq (0,${SYSTEMD})
uninstall: uninstall-service-systemd
uninstall-service-systemd:
		rm -f ${DESTDIR}${SERVICEDIR_SYSTEMD}/deadd-notification-center.service
endif


.PHONY: all clean install uninstall
