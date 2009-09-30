/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2")
*/

// javascript toBool()
function iff(obj) { return !!obj }

Module(function baseTypeSystem(l) {
  
  function asArgs(args) {
    var argsArray = Array.apply(null, args);
    argsArray.isArgsArray = true;
    return argsArray;
  }
  
  var tc = function typeCheck(argument, typeRef) {
    
  }
  
  function GetTypeName() { return this.typeName }
  
  l.type = function type(localContext, prototypeConstructor, instanceConstructor, parentType, inheritConstructorChain) {
    var typeRef;
    typeRef = function() {
      this.Type = typeRef;
      var returned = instanceConstructor.apply(this, arguments);
      if (typeRef.parentType && inheritConstructorChain) {
        var returned2;
        returned = typeof(returned2 = typeRef.parentType.instanceConstructor.apply(typeof(returned)=='undefined' ? this : returned, arguments)) != 'undefined'
          ? returned2
          : returned;
      }
      return typeof(returned)=='undefined'
        ? this
        : returned;
    };
    if (typeRef.parentType = parentType) {
      typeRef.derivesFrom(parentType);
    }
    typeRef.typeName = instanceConstructor.toString().match(functionNameExtractor)[1];
    typeRef.prototype['is'+typeRef.typeName] = true;
    prototypeConstructor.call(typeRef.prototype);
    typeRef.instanceConstructor = instanceConstructor;
    typeRef.toString = GetTypeName;
    typeRef.check = function(obj) {
      if (!(obj instanceof typeRef)) throw 'Type Check failure: '+argument+' is not a(n) '+typeRef.typeName;
    }
    localContext['is'+typeRef.typeName] = function(obj) { return obj instanceof typeRef };
    localContext[typeRef.typeName] = typeRef;
  };
  
  l.type(l, function(){
    this.toString = function() { return this.Value().toString() };
    this.Value = function() { return this.value; };
    this.toTypedString = function() { return '||'+this.Type+'|:|'+this.toString()+'||' };
    this.In = function(coll) { l.Collection.check(coll);
      return coll.has(this);
    };
  }, function Value(value){
    if (l.isValue(value)) {
      return value;
    } else if (value===null) {
      return new l.Null();
    } else {
      this.value = value;
    }
  });
  
  l.type(l, function(){}, function Any(){}, l.Value);
  
  l.type(l, function(){}, function General(){}, l.Any);
  
  l.type(l, function(){
    this.toString = function() { return this.extentName + ' { ' + this.Value() + ' }'; }
  }, function Extent(targetContext, nodeName, nodeValue){
    this.extentName = nodeName;
    this.value = nodeValue;
    return targetContext[nodeName] = this;
  }, l.Any);
  
  l.type(l, function(){
    this.value = { toString: function() { return 'null' } };
  }, function Null(){}, l.Any);
  
  l.type(l, function(){}, function Number(){}, l.General);
  
  l.type(l, function(){}, function Decimal(){}, l.General);
  l.type(l, function(){}, function Decimal9(){}, l.Decimal);
  l.type(l, function(){}, function Decimal19(){}, l.Decimal);
  l.type(l, function(){}, function Decimal28(){}, l.Decimal);
  l.type(l, function(){}, function Decimal38(){}, l.Decimal);
  
  l.type(l, function(){}, function Integer(){}, l.Decimal);
  l.type(l, function(){}, function Integer8(){}, l.Integer);
  l.type(l, function(){}, function Integer16(){}, l.Integer);
  l.type(l, function(){}, function Integer32(){}, l.Integer);
  l.type(l, function(){}, function Integer64(){}, l.Integer);
  
  l.type(l, function(){}, function Unsigned(){}, l.Integer);
  l.type(l, function(){}, function Unsigned8(){}, l.Unsigned);
  l.type(l, function(){}, function Unsigned16(){}, l.Unsigned);
  l.type(l, function(){}, function Unsigned32(){}, l.Unsigned);
  l.type(l, function(){}, function Unsigned64(){}, l.Unsigned);
  
  l.type(l, function(){}, function Scientific(){}, l.Number);
  l.type(l, function(){}, function Single(){}, l.Scientific);
  l.type(l, function(){}, function Double(){}, l.Scientific);
  
  l.type(l, function(){}, function Date(){}, l.General);
  
  l.type(l, function(){}, function DateTime(){}, l.General);
  
  l.type(l, function(){}, function DateTimeOffset(){}, l.General);
  
  l.type(l, function(){}, function Time(){}, l.General);
  
  l.type(l, function(){
    this.Count = function() { return this.count; };
    this.Plus = function(right) { l.Text.check(right); return new l.Text(this.Value() + right.Value()); };
  }, function Text(textVal){
    this.value = (textVal = typeof(textVal)=='undefined' ? '' : textVal.toString());
    this.count = textVal.length;
  }, l.General);
  
  l.type(l, function(){}, function Character(){}, l.General);
  
  l.type(l, function(){
    this.ternaryOp = function(apodosis, elseGive) {
      return this.Value() ? apodosis : elseGive;
    };
    this.negate = function() {
      return this.Value() ? new l.Logical(false) : new l.Logical(true);
    }
  }, function Logical(booleanVal){
      var argType = typeof booleanVal;
      this.value = argType=='undefined'
        ? true
        : argType=='Boolean'
          ? booleanVal
          : booleanVal instanceof l.Value
            ? booleanVal instanceof l.Logical
              ? booleanVal.value
              : booleanVal.Value() ? true : false
            : booleanVal ? true : false;
  }, l.General);
  
  l.type(l, function(){}, function Binary(){}, l.General);
  
  l.type(l, function(){}, function Byte(){}, l.General);
  
  l.type(l, function(){
    this.Equals = function(guid) { l.Guid.check(guid);
      return this.toString()==guid.toString();
    }
  }, function Guid(guidString) {
    this.value = guidString
         || __HexChar.times(8) + 
      '-' + __HexChar.times(4) + 
      '-' + __HexChar.times(4) + 
      '-' + __HexChar.times(4) + 
      '-' + __HexChar.times(12);
  }, l.General);
  
  l.NewGuid = function() { return new l.Guid() }
  
  l.type(l, function() { // Collection, a.k.a. MultiSet
    this.toString = function() {
      return '{ '+this.Values().join(', ')+' }';
    };
    this.has = function(memberInQuestion) {
      var member = new l.Value(memberInQuestion);
      for (var i=0,j=(vals=this.values).length;i<j;i++) {
        if (vals[i].toTypedString()==member.toTypedString()) {
          return true;
        }
      }
      return false;
    }
  },function Collection() {
      var values = [];
      var vals;
      for (var i=0,j=(vals=arguments[0]).length;i<j;i++) {
        values[i] = new l.Value(vals[i]);
      }
      this.values = values;
      this.Values = function() { return values };
  }, l.Any);
  
  l.type(l, function() { // Entity; Collection of Extents
    this.addField = function() {
      new l.Extent(this, arguments[0], arguments[1]);
      this.fieldNames.push(arguments[0]);   
    };
    this.FieldNames = function() {
      return new l.Collection(this.fieldNames);
    };
    this.Values = function() {
      var result=[], names;
      for (var i=0,j=(names=this.fieldNames).length;i<j;i++) {
        result[i] = this[names[i]];
      }
      return result;
    };
  }, function Entity(){
    var names = this.fieldNames = [];
  }, l.Collection);
  
  return l;
}, true, true);

testMode = true;

function runTests() {
  var outputStr = '';
  try {
    Module.Exec(function testTypeSystem(l) { with (l) {
      
      var assertCount = 0;
      l.assert = function(predicate, compareTo) {
        var actualCompareTo = typeof(compareTo)=='undefined' ? true : compareTo;
        outputStr += (predicate.toString()==actualCompareTo.toString() ? 'ok '+assertCount++ : 'nok '+(assertCount++)+': '+[predicate,actualCompareTo])+"\n";
      };
      
      var guid = new Guid();
      
      assert(guid.toString().length,36);
      assert(guid.Type,'Guid');
      assert(guid instanceof Any);
      assert(guid instanceof Guid);
      assert(iff(guid.isInteger),false);
      assert(iff(guid.isGuid));
      assert(isGuid(guid));
      
      var entity1 = new Entity();
      entity1.addField('blah', new Value('haha'));
      entity1.addField('foo', new Value('bar'));
      assert(entity1.FieldNames(),'{ blah, foo }');
      assert(entity1,'{ blah { haha }, foo { bar } }');
      assert(entity1.foo,'foo { bar }');
      
      var nullv = new Value(null);
      assert(nullv,'null');
      
      var coll1 = new Collection([1,2,3,4]);
      assert(coll1,'{ 1, 2, 3, 4 }');
      assert(coll1.has(1));
      assert(coll1.has(5),false);
      assert(coll1.has(null),false);
      assert(coll1.has(coll1),false);
      assert(new Value(3).In(coll1));
      
      var Test = l;
      
      type(Test, function() {
        this.getX = function () {
            return this.x
        }
        this.setX = function (x) {
            this.x = x
        }
        this.getY = function () {
            return this.y
        }
        this.setY = function (y) {
            this.y = y
        }
        this.clear = function () {
            this.setX(0)
            this.setY(0)
        }
      }, function Point(x, y) {
        this.x = x || 0
        this.y = y || 0
      });
      
      type(Test, function() {
        this.getZ = function () {
          return this.z
        }
        this.setZ = function (z) {
          this.z = z
        }
        this.clear = function () {
          this.Type.parentType.prototype.clear.call(this)
          this.setZ(0);
        }
      }, function Point3D (x, y, z) {
        this.z = z || 0
      }, Point, true);
      
      var p1 = new Point(4, 2);
      assert(p1.getX(),4);
      assert(p1 instanceof Point);
      var p2 = new Point3D(5,7,11);
      assert(p2 instanceof Point);
      assert(p2 instanceof Point3D);
      assert(p2.getX(),5);
      assert(p2.getY(),7);
      assert(p2.getZ(),11);
      p2.clear();
      assert(p2.x==0 && p2.y==0 && p2.z==0);
      
      return l;
    }});
  } catch(e) {
    return outputStr + "\n" + e;
  }
  return outputStr.chop();
}


































