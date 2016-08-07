Ext.require([
    'Ext.tree.*',
    'Ext.data.*',
    'Ext.layout.container.HBox',
    'Ext.window.MessageBox'
]);

Ext.define('Item', {
    extend: 'Ext.data.Model',
    fields: ['text', 'canDropOnFirst', 'canDropOnSecond']
})

Ext.onReady(function() {
 
  
  var today = new Date();
  
    var store2 = new Ext.data.TreeStore({
        model: 'Item',
        root: {
            text: 'Portefeuille',
            expanded: true,
            children: [{
                text: 'Portefeuille',
                children: [],
                expanded: true
            }]
        }
    });

   var treePanel = new Ext.panel.Panel({
        renderTo: 'tree-div',
        width: 300,
        height: 200,
        layout: {
            type: 'hbox',
            align: 'stretch'
        },
        defaultType: 'treepanel',
        defaults: {
            rootVisible: false,
            flex: 1
        },
        items: [{
            title: 'Source',
            store: {
      root: {
        expanded: true
      }
    },
            viewConfig: {
                plugins: {
                   ptype: 'treeviewdragdrop',
                   enableDrag: true,
                   enableDrop: false
                }
            }
        }, {
            title: 'Destination',
            store: store2, 
            viewConfig: {
                plugins: {
                   ptype: 'treeviewdragdrop',
                   enableDrag: false,
                   enableDrop: true,
                   appendOnly: true
                },
                listeners: {
                    nodedragover: function(targetNode, position, dragData){
                        var rec = dragData.records[0],
                            isFirst = targetNode.isFirst(),
                            canDropFirst = rec.get('canDropOnFirst'),
                            canDropSecond = rec.get('canDropOnSecond');
                            
                        return isFirst ? canDropFirst : canDropSecond;
                         addWorkspace(store2.data.id.substring(7));
                    }
                }
            }
        }]
    });
     
    }      
}); 

 

   
  var myToolbar = Ext.create('Ext.toolbar.Toolbar', {
    "items" :['->',{
      xtype: "combobox",
      editable: false,
        store: {
        fields: ['fun', 'name'],
          data : [
            {"fun":"plotDensityPortefeuilleByShare","name":"PORTEFEUILLE: Densité de la Plus-Value par Action"},
            {"fun":"getPortefeuilleValue","name":"PORTEFEUILLE: Valeur du Portefeuille"},
          ]          
        },
        queryMode: 'local',
        displayField: 'name',
        valueField: 'fun',
        value: "smoothplot",
        id: "graphtype",
        iconCls: 'chartIcon'
      }, {
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
      },{
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
        id: "skewnessBtn",
        enableToggle: true,
        text: "Skewness",
        iconCls: 'chartIcon'
},{
        xtype: "button",
        id: "kurtosisBtn",
        enableToggle: true,
        text: "Kurtosis",
        iconCls: 'chartIcon'
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
      items : [ treePanel ]
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
      updatestart(data.start);
      updateend(data.end);
    }
  }
  
  function loadplot(){
    var portefeuille = Ext.getCmp('workspace-panel').getActiveTab().title;
    var from = Ext.getCmp("startdate").picker.getValue();
    var to = Ext.getCmp("enddate").picker.getValue()
    var type = Ext.getCmp("graphtype").getValue();
    
    
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
    var idportefeuille = Ext.getCmp('workspace-panel').getActiveTab().el.id;
    var req = $("#" + idportefeuille + "-innerCt").rplot("plotwrapper", {
      ticker : portefeuille, 
      from : datetostring(from), 
      to : datetostring(to), 
      type : type, 
    }).fail(function(){
      alert("Failed to plot stock: " + req.responseText)
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
