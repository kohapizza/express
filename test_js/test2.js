function toggle(id){
    var objID1 = document.getElementById( id + "layerA" );
    var objID2 = document.getElementById( id + "layerB" );
    var buttonID = document.getElementById( id + "button" );
    if(objID1.className=='close') {
        objID1.style.display = 'block';
        objID1.className = 'open';
        objID2.style.display = 'none';
        objID2.className = 'close';
        buttonID.innerHTML = "-";
    }else{
        objID1.style.display = 'none';
        objID1.className = 'close';
        objID2.style.display = 'block';
        objID2.className = 'open';
        buttonID.innerHTML = "+";
    }
};

