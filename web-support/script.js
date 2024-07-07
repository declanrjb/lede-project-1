function toggleLight(light) {
    if (light.getAttribute('active') == 'true') {
        light.setAttribute('active',false)
        light.style.backgroundColor = 'white'
    } else {
        light.setAttribute('active',true)
        light.style.backgroundColor = '#fce694'
    }
}

function toggleRecursive() {
    setTimeout(function() {
        for (var j=0; j<lights.length; j++) {
            toggleLight(lights[j]);
        }
        toggleRecursive()
    }, 200);
}

var lights = document.querySelectorAll('.marquis-light');
console.log(lights);

for (var i=0; i<lights.length; i++) {
    if ((i % 2) == 0) {
        lights[i].setAttribute('active',true);
    } else {
        lights[i].setAttribute('active',false);
    }
}

toggleRecursive()
