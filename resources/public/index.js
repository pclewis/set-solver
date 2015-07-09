var paper = Raphael( $('#result')[0], 640, 480);

function snapshot() {
    var camera = $('#camera')[0], preview = $('#preview');
    var width = camera.videoWidth, height = camera.videoHeight;
    preview.attr('width', width).attr('height', height);
    preview[0].getContext('2d').drawImage(camera, 0, 0, width, height);
    //var data_uri = preview[0].toDataURI('image/jpg');

    var formData = new FormData();
    formData.append('file', preview[0].mozGetAsFile('file', 'image/jpg'), 'image');
    $.ajax( {url: '/solve',
             type: 'POST',
             data: formData,
             processData: false,
             contentType: false,
             dataType: "json",
             success: r});
}

if (navigator.mozGetUserMedia) {
    navigator.mozGetUserMedia(
        {video: {optional: {
            minWidth: 1280,
            minHeight: 720,
            facingMode: 'environment'}}},
        function(stream) {
            var camera = $('#camera')[0];
            camera.src = window.URL.createObjectURL(stream);
            camera.play();
            setInterval( snapshot, 2000 );
        },
        function(e) { console.log(e); }
    );
}


function r(data) {
    var result = $('#result'), preview = $('#preview');
    paper.setSize( preview.width(), preview.height() );
    //paper.setViewBox(0,0, preview.width() , preview.height(),true);
    paper.image( preview[0].toDataURL("image/jpeg"), 0, 0, preview.width(), preview.height() );
    var cardmap = {};
    data.cards.forEach( function(card) {
        console.log(card);
        var rect = paper.rect( card.bb.x, card.bb.y, card.bb.width, card.bb.height, 3 );
        rect.attr('stroke', '#fff');
        var text = paper.text( card.bb.x, card.bb.y, card.id );
        text.attr('fill', '#fff').attr('font-size', 100);
        cardmap[card.id] = card;
    });

    data.sets.forEach( function(set, i) {
        var prev = null;
        var color = tinycolor( {h: 0, s: 0.5, l: 0.5} );
        color.spin( i * 50 );
        set.forEach( function(c) {
            var card = cardmap[c.id];
            var circle = paper.circle( card.bb.x + (i * 20), card.bb.y + (i * 20), 10 );
            circle.attr('fill', color.toHexString());
            if (prev) {
                var path = paper.path( "M" + (prev.bb.x + (i * 20)) + "," + (prev.bb.y + (i * 20)) +
                                       "L" + (card.bb.x + (i * 20)) + "," + (card.bb.y + (i * 20)));
                path.attr('stroke', color.toHexString()).attr('stroke-width', '5');
            }
            prev = card;
        });
    });
}
