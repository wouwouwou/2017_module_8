grammar OperAttr;

import OperVocab;

@header{import pp.block3.cc.antlr.Type;}

@members {
	private Type getType(String input) {
			switch (input) {
				case "NUM":
					return Type.NUM;
				case "BOOL":
					return Type.BOOL;
				case "STR":
					return Type.STR;
				default:
					return Type.ERR;
			}
		}

		private String getHatString(Type t0, String v0, Type t1, String v1) {
			if (t0 == Type.NUM && t1 == Type.NUM) {
				int base = Integer.parseInt(v0);
				int exp = Integer.parseInt(v1);
				return new Integer(new Double(Math.pow(base, exp)).intValue()).toString();
			} else if (t0 == Type.STR && t1 == Type.NUM) {
				int exp = Integer.parseInt(v1);
				String result = "";
				String temp = v0.substring(1,v0.length()-1);
				for (int i = 0; i < exp; i++) {
					result = result + temp;
				}
				return '"' + result + '"';
			} else {
				return "";
			}
		}

		private Type getHatType(Type t0, Type t1) {
			if (t0 == Type.NUM && t1 == Type.NUM) {
				return Type.NUM;
			} else if (t0 == Type.STR && t1 == Type.NUM) {
				return Type.STR;
			} else {
				return Type.ERR;
			}
		}

		private String getPlusString(Type t0, String v0, Type t1, String v1){
			if (t0 == Type.NUM && t1 == Type.NUM) {
	        	int x = Integer.parseInt(v0);
	        	int y = Integer.parseInt(v1);
	        	return new Integer((x+y)).toString();
	        } else if (t0 == Type.STR && t1 == Type.STR) {
	        	return '"' + v0.substring(1, v0.length()-1) + v1.substring(1, v1.length() -1) + '"';
	        } else if (t0 == Type.BOOL && t1 == Type.BOOL) {
				return new Boolean(new Boolean(v0) || new Boolean(v1)).toString();
	        } else {
	        	return "";
	        }
		}

		private Type getPlusType(Type t0, Type t1) {
			if (t0 == t1) {
				return t0;
			} else {
				return Type.ERR;
			}
		}

		private String getEqualsString(Type t0, String v0, Type t1, String v1){
			if (t0 == t1) {
				return new Boolean(v0.equals(v1)).toString();
			} else {
				return "";
			}
		}

		private Type getEqualsType(Type t0, Type t1) {
			if (t0 == t1) {
				return Type.BOOL;
			} else {
				return Type.ERR;
			}
		}
}

t returns [Type type, String value]
	: 	t0=t HAT t1=t
		{ $type = getHatType($t0.type, $t1.type); }
		{ $value = getHatString($t0.type, $t0.value, $t1.type, $t1.value); }
	|	t0=t PLUS t1=t
		{ $type = getPlusType($t0.type, $t1.type); }
		{ $value = getPlusString($t0.type, $t0.value, $t1.type, $t1.value); }
	|	t0=t EQUALS t1=t
		{ $type = getEqualsType($t0.type, $t1.type); }
		{ $value = getEqualsString($t0.type, $t0.value, $t1.type, $t1.value); }
	|	LPAR t0=t RPAR
		{ $type = $t0.type; }
		{ $value = $t0.value; }
	|	NUM
		{ $type = getType("NUM"); }
		{ $value = $NUM.text; }
	|	BOOL
		{ $type = getType("BOOL"); }
        { $value = $BOOL.text; }
	|	STR
		{ $type = getType("STR"); }
        { $value = $STR.text; }
	;