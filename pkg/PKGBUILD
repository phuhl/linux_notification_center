# Maintainer: Philipp Uhl <philipp.uhl@rwth-aachen.de>

pkgname=deadd-notification-center
pkgver=1.0
pkgrel=1
pkgdesc="Customizable notification-daemon with notification center"
url="https://github.com/phuhl/linux_notification_center"
license=("BSD")
arch=('x86_64')
depends=('gobject-introspection-runtime' 'gtk3')
makedepends=('stack' 'cairo' 'pango' 'gobject-introspection')
source=("${pkgname}-${pkgver}.tar.gz::https://github.com/phuhl/linux_notification_center/archive/v${pkgver}.tar.gz")
prepare() {
    tar -zxvf "${pkgname}-${pkgver}.tar.gz"
}
build() {
    cd "linux_notification_center-${pkgver}"
    make
}

package() {
    cd "linux_notification_center-${pkgver}"
    make DESTDIR="$pkgdir" install
}
md5sums=('e38a353113aa938fe97ec06fcd4d0966')
