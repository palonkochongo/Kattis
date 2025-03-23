

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

class Main{

    public static boolean bothAlive(int[] health, int knight1, int knight2){
        return (health[knight1]>0 && health[knight2]>0);
    }




    public static void main(String[] args) throws IOException {
        BufferedReader buf = new BufferedReader(new InputStreamReader(System.in));
        String line1 = buf.readLine();
        int n = Integer.parseInt(line1);

        int[] health = new int[n];
        int[] strength = new int[n];
        for(int i=0 ; i<n ; i++){
            String line2 = buf.readLine();
            String[] tokens = line2.split(" ");
            health[i]= Integer.parseInt(tokens[0]);
            strength[i]= Integer.parseInt(tokens[1]);
        }


        int knight1 = 0;
        int knight2 = 1;
        int knightCounter = 1;


        while(knightCounter < n){
            if(knight1<knight2){
                if(bothAlive(health, knight1, knight2)) health[knight2] -= strength[knight1];
                if(bothAlive(health, knight1, knight2)) health[knight1] -= strength[knight2];
            }
            else if(knight2<knight1){
                if(bothAlive(health, knight1, knight2)) health[knight1] -= strength[knight2];
                if(bothAlive(health, knight1, knight2)) health[knight2] -= strength[knight1];
            }

            if(health[knight2] <= 0) knight2 = ++knightCounter;
            if(health[knight1] <= 0) knight1 = ++knightCounter;
        }

        if(knight1 == n) System.out.println(knight2+1);
        if(knight2 == n) System.out.println(knight1+1);

    }
}
