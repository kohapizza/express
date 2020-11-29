function show(input1, input2){
    var objID1 = document.getElementById( "layer_" + input1 );
    var objID2 = document.getElementById( "layer_" + input2 );
    var buttonID = document.getElementById( "category_" + input1 );
    var click = true;
    if(objID1.className=='close') {
        objID1.style.display = 'block';
        objID1.className = 'open';
        objID2.style.display = 'none';
        objID2.className = 'close';
        changebtn(click, buttonID1);
        click = false;
    }else{
        objID1.style.display = 'none';
        objID1.className = 'close';
        objID2.style.display = 'block';
        objID2.className = 'open';
        click = false;
        changebtn(click, buttonID1);
        click = true;
    }

};

function changebtn(btn,category){
 if(btn == true){
    category.innerHTML = "-";
 }else{
    category.innerHTML = "+";
 } 
};



