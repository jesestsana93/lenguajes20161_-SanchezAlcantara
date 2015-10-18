
/**
* Este programa de factorial hace alusion a la prueba de que Java es gloton del ejercicio 2 de 
* la tarea 2
*/
public class Prueba{
	public int factorial (int n){
		if(n == 0){
			System.out.println("Valor de n: "+n);
			return 1;
		}else{
			System.out.println("Valor de n: "+n);
			return(n * (factorial (n -1)) );
		}
	}

public double divide(int n, int m){
	System.out.println("Divide: "+n+" / "+m);
	return n/m;
}


	public static void main (String args []){
		Prueba p = new Prueba();
		//Evaluación glotona, resuelve primero la llamada de factorial
		// y luego hace la división entre cero
		p.divide( p.factorial(5), 0);

	}



}
