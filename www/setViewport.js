window.onload = function () {
    let vp = document.getElementById('vp')
    if (window.innerWidth <= 450) {
        vp.setAttribute('content', 'width=600')
    }
}
