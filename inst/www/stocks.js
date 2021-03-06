Ext.Loader.setConfig({
  disableCaching: false
});

Ext.onReady(function() {
  Ext.require([
    'Ext.tree.*',
    'Ext.data.*',
    'Ext.window.MessageBox'
]);

  
  var today = new Date(); //définition de la date today qui est la date actuelle 
  
  // création de l'arbre contenant les actions
   var treePanel = new Ext.tree.TreePanel({
    id: 'tree-panel',
    iconCls: 'chartIcon',
    title: 'by Index',
    region: 'center',
    title: "Actions",
    height: 300, //réglage de la hauteur
    border: false,
    autoScroll: true, //permet de dérouler l'arbre si la fenêtre est trop petite.
    lazyRender:true,
    animate: true,
    containerScroll: true,
    enableDrag: true,
    dragConfig: {ddGroup: 'DragDrop' },
    autoWidth: true, // réglage de la largeur en fonction des autres fenêtres de la page
    
    // tree-specific configs:
    rootVisible: false, //la racine de l'arbre n'est pas visible 
    lines: false,
    singleExpand: true,
    useArrows: true,
    //utilisation des données définies de la ligne 320 à 333
    store: {
      root: {
        expanded: true
      }
    },
    
    // si vous appuyez sur une action de l'arbre, un nouveau tracé apparaît grâce à la méthode addWorkspace défini plus loin
    listeners: {
      itemdblclick: function(s, r){
        if(r.data.leaf){
          addWorkspace(r.data.id.substring(7));
        }
      },
     
    }      
}); 

 

   
  var myToolbar = Ext.create('Ext.toolbar.Toolbar', {
    containerScroll: true,
    autoScroll: true,// permet d'avoir accès à toutes les options
    //définitions des objects de la toolbar
    "items" :['->',{
      xtype: "combobox",
      editable: false,
        store: {
        fields: ['fun', 'name'],
          data : [
            {"fun":"smoothplot", "name":"ACTION: Smooth Plot"},
            {"fun":"highlowplot", "name":"ACTION: High/Low Plot"},
            {"fun":"areaplot", "name":"ACTION: Area Plot"},
            {"fun":"plotDensity", "name":"ACTION: Densité"},
            {"fun":"getPlotCapitalGain", "name":"ACTION: Plus-Value"},
            {"fun":"densityGain", "name":"ACTION: Densité de la Plus-Value"},
            {"fun":"plotDensityPortefeuilleByShare","name":"PORTEFEUILLE: Densité de la Plus-Value par Action"},
            {"fun":"getPortefeuilleValue","name":"PORTEFEUILLE: Valeur du Portefeuille"},
            {"fun":"varianceGain","name":"PORTEFEUILLE: Variance/Gain des actions du portefeuille"},
            {"fun":"interpolation","name":"ACTION: Interpolation"},
            {"fun":"interpolation2","name":"ACTION: Interpolation2"},
          ]          
        },
        queryMode: 'local',
        displayField: 'name',
        valueField: 'fun',
        value: "smoothplot",
        id: "graphtype",
        iconCls: 'chartIcon'
      }, { //ajout des dates de début et de fin
        text: 'Date de Début: 2015-01-01',
        id: 'startdatetext',
        iconCls: 'calendarIcon',
        menu: {
          xtype: 'datemenu',
          minValue: new Date('01/01/2000'),
          id: 'startdate',
          value: new Date('01/01/2015')
        }
      }, {
        text: 'Date de Fin: ' + today.getFullYear() + "-" + (today.getMonth()+1) + "-" + today.getDate(),
        id: 'enddatetext',
        iconCls: 'calendarIcon',      
        menu: {
          xtype: 'datemenu',
          minValue: new Date('01/01/2000'),
          id: 'enddate',
          value: new Date()
        }
      },{// ajout des différents boutons
      
        xtype: "button",
        id: "currentBtn",
        enableToggle: true,
        text: "Valeur Actuelle",
        iconCls: 'chartIcon'
      },{
        xtype: "button",
        id: "moyenneBtn",
        enableToggle: true,
        text: "Moyenne",
        iconCls: 'chartIcon'
      },{
        xtype: "button",
        id: "varianceBtn",
        enableToggle: true,
        text: "Variance",
        iconCls: 'chartIcon'
},{
        xtype: "button",
        id: "maxBtn",
        enableToggle: true,
        text: "Maximum",
        iconCls: 'chartIcon'
},{
        xtype: "button",
        id: "minBtn",
        enableToggle: true,
        text: "Minimum",
        iconCls: 'chartIcon'
},{
        xtype: 'textfield',
        id: 'degre',
        fieldLabel: 'Degré du polynome',
        value:"",
        allowBlank: true
    },{// ajout des textfields
        xtype: 'textfield',
        id: 'action',
        fieldLabel: 'Actions du Portefeuille',
        value:"",
        allowBlank: true
    },{
        xtype: 'textfield',
        id: 'nombre',
        fieldLabel: 'Proportions des Actions',
        value:"",
        allowBlank: true
    },{
        xtype: 'textfield',
        id: 'infla',
        fieldLabel: 'Taux d actualisation moyen par an',
        value:"",
        allowBlank: true
    }]
  });

  var workspacePanel = new Ext.TabPanel({
    activeTab: 0,
    createTab: addWorkspace,
    id: 'workspace-panel',
    region: 'center',
    margins: '2 5 5 0',  
    height: 350,
    border: false,    
    tabPosition: 'bottom',
    items: [{
      iconCls: 'chartIcon',
      closable: false,
      border: false,
      title: "Help",
      anchor: '-10, -262',
      contentEl: "helpdiv"
    }],
    listeners: {
      "tabchange" : function(tabPanel, newtab){
        updatemenu();
      }
    },
    tbar: myToolbar  
  });
  
  var detailsPanel = Ext.Panel({
    id: 'details-panel',
    split: true,      
    height: 205,
    minSize: 150,   
    title: 'Details',
    region: 'south',    
    bodyStyle: 'padding-bottom:15px;background:#eee;'
}); 

  new Ext.Viewport({
    id : 'viewport',
    layout : 'border',
    items : [ {
      layout : 'border',
      id : 'layout-browser',
      region : 'west',
      border: false,
      split:true,
      margins: '2 0 5 5',
      width: 200,
      minSize: 100,
      maxSize: 500,
      items : [ treePanel,detailsPanel ]
    }, workspacePanel ],
    renderTo : Ext.getBody()
  });
  
  function updatestart(date){
    Ext.getCmp("startdatetext").setText("Date de Début: " + datetostring(date));
  }
  
  function updateend(date){
    Ext.getCmp("enddatetext").setText("Date de Fin: " + datetostring(date));
  }
  
  Ext.getCmp("startdate").picker.on("select", function(picker, date){
    updatestart(date);
    loadplot();
  });
  
  Ext.getCmp("enddate").picker.on("select", function(picker, date){
    updateend(date);
    loadplot();
  });  
  
  
  Ext.getCmp("currentBtn").on("click", function(){
    loadplot();
  });
  
    Ext.getCmp("moyenneBtn").on("click", function(){
    loadplot();
  });
  
   Ext.getCmp("varianceBtn").on("click", function(){
    loadplot();
});
  Ext.getCmp("maxBtn").on("click", function(){
    loadplot();
  });
   Ext.getCmp("minBtn").on("click", function(){
    loadplot();
  });
 
  Ext.getCmp("graphtype").on("select", function(){
    loadplot();
  });
  
  
  function addWorkspace(symbol){
    workspacePanel.add({
      iconCls: 'chartIcon',
      closable: true,
      title: symbol,
      border: false,
      data : {
        type : Ext.getCmp("graphtype").getValue(),
        current : Ext.getCmp("currentBtn").pressed,
        moyenne : Ext.getCmp("moyenneBtn").pressed,
        variance : Ext.getCmp("varianceBtn").pressed,
        max : Ext.getCmp("maxBtn").pressed,
        min : Ext.getCmp("minBtn").pressed,
        start : Ext.getCmp("startdate").picker.getValue(),
        end : Ext.getCmp("enddate").picker.getValue()
       }
    }).show();
    loadplot();
    }
  
  function updatemenu(){
    var data = Ext.getCmp('workspace-panel').getActiveTab().data;
    if(data){
      
      Ext.getCmp("startdate").picker.setValue(data.start);
      Ext.getCmp("enddate").picker.setValue(data.end);
      Ext.getCmp("graphtype").setValue(data.type);
      Ext.getCmp("currentBtn").toggle(data.current); 
      Ext.getCmp("moyenneBtn").toggle(data.moyenne);
      Ext.getCmp("varianceBtn").toggle(data.variance);
      Ext.getCmp("minBtn").toggle(data.min);
      Ext.getCmp("minBtn").toggle(data.min);
      updatestart(data.start);
      updateend(data.end);
    }
  }
  
  function loadplot(){
    var symbol = Ext.getCmp('workspace-panel').getActiveTab().title;
    var from = Ext.getCmp("startdate").picker.getValue();
    var to = Ext.getCmp("enddate").picker.getValue();
    var type = Ext.getCmp("graphtype").getValue();
    var portefe = Ext.getCmp("action").getValue();
    var nomb  = Ext.getCmp("nombre").getValue();
    var deg = Ext.getCmp("degre").getValue();
    var infla = Ext.getCmp("infla").getValue();
    var current = Ext.getCmp("currentBtn").pressed;
    var gain = Ext.getCmp("currentBtn").pressed;
    var moyenne = Ext.getCmp("moyenneBtn").pressed;
    var variance = Ext.getCmp("varianceBtn").pressed;
    var min = Ext.getCmp("minBtn").pressed;
    var max = Ext.getCmp("maxBtn").pressed;
    
    //don't plot help tab
    if(symbol == "Help"){
      return;
    }
    
    //save settings in tab
    Ext.getCmp('workspace-panel').getActiveTab().data = {
      start: from,
      end: to,
      type: type,
      current: current    
    }  
    
    //request plot using OpenCPU library
    var id = Ext.getCmp('workspace-panel').getActiveTab().el.id;
    var req = $("#" + id + "-innerCt").rplot("plotwrapper", {
      portefe : portefe,
      nomb : nomb,
      deg : deg,
      infla: infla,
      ticker : symbol,
      from : datetostring(from), 
      to : datetostring(to), 
      type : type, 
      current : current,
      moyenne : moyenne,
      variance : variance,
      min : min,
      max : max,
    }).fail(function(){
      if(symbol=="portefeuille"){
        alert("Avez vous entré les actions dans le portefeuille, choisi le bon type de graphe, ou les bons symboles?" + req.responseText)
      }
      else{alert("Failed to plot stock: " + req.responseText)}
    });
  }
  
  function datetostring(date){
    var dd = date.getDate();
    var mm = date.getMonth()+1;
    var yyyy = date.getFullYear();          
    return yyyy + "-" + mm + "-" + dd;    
  }
  
  //this function gets a list of stocks to populate the tree panel
  function loadtree(){
    var req = ocpu.rpc("listbyindustry", {}, function(data){
      Ext.getCmp("tree-panel").getStore().setProxy({
        type : "memory",
        data : data,
        reader : {
          type: "json"
        }
      });
      Ext.getCmp("tree-panel").getStore().load();
    }).fail(function(){
      alert("Failed to load stocks: " + req.responseText);
    });
  }

  //init
  loadtree();
});
