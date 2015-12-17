/*
    Nikola Jovanovic - Matematicka gimnazija 4D
    Maturski rad iz informatike:
    Lambda racun i funkcionalno programiranje
    uz dodatni projekat : izrada kompajlera u programskom jeziku Haskell
    4.2015.

    Implementacija virtuelne stek masine (VSM)
*/

// 16 instrukcija niskog nivoa
// notacija:
// n : celobrojna promenljiva, s : string ime promenljive
// S1 : vrednost na vrhu steka, S2 : vrednost prvog elementa ispod vrha steka

#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <unordered_map>
#include <vector>
#include <string>
#define MAX_INSTS 65535 // maksimalan broj instrukcija
#define MAX_SSZ 1024 // maksimalna velicina steka (indeksi 1 - 1024)
#define MAX_VARS 1024 // maksimalan broj promenljivih
#define NOP 0x00 // NOP : prazna instrukcija
#define PUSH 0x01 // PUSH n : push n na vrh steka
#define POP 0x02 // POP : pop steka
#define LOAD 0x03 // LOAD s : push vrednosti promenljive s na vrh steka
#define STORE 0x04 // STORE s : postavljanje vrednosti promenljive s na S1 i pop steka
#define ADD 0x05 // ADD : push S2 + S1 na vrh steka, pop S1 i S2
#define SUB 0x06 // SUB : push S2 - S1 na vrh steka, pop S1 i S2
#define MUL 0x07 // MUL : push S2 * S1 na vrh steka, pop S1 i S2
#define OR 0x08 //  OR  : push S2 | S1 na vrh steka, pop S1 i S2
#define AND 0x09 // AND : push S2 & S1 na vrh steka, pop S1 i S2
#define NOT 0x0A // NOT : push !S1 na vrh steka, pop S1
#define JMP 0x0B // JMP n : skok na instrukciju n
#define JZ 0x0C // JZ n : skok na instrukciju n ako je S1=0 (Jump Zero) i pop steka
#define JP 0x0D // JP n : skok na instrukciju n ako je S1>0 (Jump Plus) i pop steka
#define JM 0x0E // JM n : skok na instrukciju n ako je S1<0 (Jump Minus) i pop steka
#define HALT 0x0F // HALT : zaustavljanje programa

using namespace std;

// struktura koja predstavlja VSM
struct vsm_struct
{
    int s[MAX_SSZ+1]; // stek implementiran kao niz (deo memorije)
    bool stackOverflow; // stek, kao i u realnoj masini, moze da prekoraci memoriju

    int SP; // pokazivac na vrh steka
    int PC; // program counter

    // struktura koja predstavlja instrukciju
    struct instruction
    {
        int op; // operacioni kod
        int arg; // dodatni argument (konstanta / indeks promenljive)
    };

    // program i velicina programa
    instruction program[MAX_INSTS+1];
    int progSz;

    // struktura koja predstavlja promenljivu
    struct variable
    {
        string name; // ime promenljive
        int val; // vrednost promenljive
    };

    // idxs je mapa koja svakoj promenljivoj dodeljuje jedinstveni indeks
    // imena i vrednosti promenljivih cuvamo u nizu vars

    unordered_map<string, int> idxs;
    variable vars[MAX_VARS+1];
    int varsSz;

    vsm_struct()
    {
        // konstruktor koji postavlja pocetne vrednosti
        progSz = 0;
        varsSz = 0;
        SP = 0; // stek pocinje od indeksa 1
        PC = 1; // prva instrukcija ima indeks 1
        stackOverflow = false; // na pocetku je stek u redu
    }

    // pomocna funkcija koja za ime promenljive vraca njen indeks
    // ako se vec pojavila ili joj dodeljuje novi indeks
    int getIdx(string varName)
    {
       if(idxs[varName] == 0)
       {
           idxs[varName] = ++varsSz;
           vars[varsSz].name = varName;
           vars[varsSz].val = 0;
       }
       return idxs[varName];
    }

    // ucitavanje instrukcije koja ne nosi argumente
    void LoadNoArg(int opCode)
    {
        program[++progSz].op = opCode;
    }

    // ucitavanje instrukcije koja ima string kao argument
    void LoadStrArg(int opCode)
    {
        string strArg;
        int intArg;
        cin >> strArg;
        intArg = getIdx(strArg);
        program[++progSz].op = opCode;
        program[progSz].arg = intArg;
    }

    // ucitavanje komande koja ima int kao argument
    void LoadIntArg(int opCode)
    {
        int intArg;
        cin >> intArg;
        program[++progSz].op = opCode;
        program[progSz].arg = intArg;
    }

    // ucitavanje programa
    void LoadProgram()
    {
        string command;

        // za svaku pojedinacno ucitanu komanda se poziva
        // odgovarajuca funkcija za ucitavanje po op kodu

        while( cin >> command )
        {
            if(command == "NOP") LoadNoArg(0x00);
            else if(command == "PUSH") LoadIntArg(0x01);
            else if(command == "POP") LoadNoArg(0x02);
            else if(command == "LOAD") LoadStrArg(0x03);
            else if(command == "STORE") LoadStrArg(0x04);
            else if(command == "ADD") LoadNoArg(0x05);
            else if(command == "SUB") LoadNoArg(0x06);
            else if(command == "MUL") LoadNoArg(0x07);
            else if(command == "OR") LoadNoArg(0x08);
            else if(command == "AND") LoadNoArg(0x09);
            else if(command == "NOT") LoadNoArg(0x0A);
            else if(command == "JMP") LoadIntArg(0x0B);
            else if(command == "JZ") LoadIntArg(0x0C);
            else if(command == "JP") LoadIntArg(0x0D);
            else if(command == "JM") LoadIntArg(0x0E);
            else if(command == "HALT") LoadNoArg(0x0F);
        }
    }

    // stampanje stanja memorije
    void PrintMemoryStatus()
    {
        cout<<"-----------------"<<endl;
        cout<<"Stanje memorije:"<<endl;
        cout<<"Velicina programa = "<<progSz<<endl;
        cout<<"Broj promenljivih = "<<varsSz<<endl;
        cout<<"Program counter = "<<PC<<endl;
        cout<<"Pokazivac na vrh steka = "<<SP<<endl;
        for(int i=1; i<=varsSz; i++)
        {
            cout<<vars[i].name<<" = "<<vars[i].val<<endl;
        }
        cout<<"-----------------"<<endl;
    }

    // izvrsavanje svih 16 instrukcija

    void DoNop()
    {
        PC++;
        return;
    }
    void DoPush(int arg)
    {
        if(SP == MAX_SSZ)
        {
            stackOverflow = true;
            return;
        }

        s[++SP] = arg;
        PC++;
        return;
    }
    void DoPop()
    {
        SP--;
        PC++;
        return;
    }
    void DoLoad(int arg)
    {
        if(SP == MAX_SSZ)
        {
            stackOverflow = true;
            return;
        }

        s[++SP] = vars[arg].val;
        PC++;
        return;
    }
    void DoStore(int arg)
    {
        vars[arg].val = s[SP--];
        PC++;
        return;
    }
    void DoAdd()
    {
        s[--SP] += s[SP+1];
        PC++;
        return;
    }
    void DoSub()
    {
        s[--SP] -= s[SP+1];
        PC++;
        return;
    }
    void DoMul()
    {
        s[--SP] *= s[SP+1];
        PC++;
        return;
    }
    void DoOr()
    {
        s[--SP] |= s[SP+1];
        PC++;
        return;
    }
    void DoAnd()
    {
        s[--SP] &= s[SP+1];
        PC++;
        return;
    }
    void DoNot()
    {
        s[SP] = !s[SP];
        PC++;
        return;
    }
    void DoJmp(int arg)
    {
        PC = arg;
        return;
    }
    void DoJz(int arg)
    {
        if(s[SP--] == 0)
            PC = arg;
        else
            PC++;
        return;
    }
    void DoJp(int arg)
    {
        if(s[SP--] > 0)
            PC = arg;
        else
            PC++;
        return;
    }
    void DoJm(int arg)
    {
        if(s[SP--] < 0)
            PC = arg;
        else
            PC++;
        return;
    }
    void DoHalt()
    {
        cout<<"Program se zaustavio na instrukciji br. "<< PC << "." <<endl;
        return;
    }

    // izvrsavanje programa

    void ExecuteProgram()
    {
        while(1)
        {
            // izvrsavanje instrukcije na koju pokazuje PC
            // zavisno od op koda poziva se odgovarajuca
            // funkcija koja izvrsava instrukciju

            switch(program[PC].op)
            {
                case(0x00) : DoNop(); break;
                case(0x01) : DoPush(program[PC].arg); break;
                case(0x02) : DoPop(); break;
                case(0x03) : DoLoad(program[PC].arg); break;
                case(0x04) : DoStore(program[PC].arg); break;
                case(0x05) : DoAdd(); break;
                case(0x06) : DoSub(); break;
                case(0x07) : DoMul(); break;
                case(0x08) : DoOr(); break;
                case(0x09) : DoAnd(); break;
                case(0x0A) : DoNot(); break;
                case(0x0B) : DoJmp(program[PC].arg); break;
                case(0x0C) : DoJz(program[PC].arg); break;
                case(0x0D) : DoJp(program[PC].arg); break;
                case(0x0E) : DoJm(program[PC].arg); break;
                case(0x0F) : DoHalt(); return;
            }

            // prekoracenje steka
            if(stackOverflow)
            {
                cout<<"Doslo je do prekoracenja steka na instrukciji br. "<< PC << "." <<endl;
                return;
            }
        }
    }

};

int main()
{
    // priprema ulaznih fajlova
    freopen("out/program.vsm", "r", stdin);
    freopen("out/vsm_out.txt", "w", stdout);

    // inicijalizacija VSM - a, ucitavanje i izvrsenje programa
    vsm_struct vsm;
    vsm.LoadProgram();
    vsm.ExecuteProgram();

    // ispis stanja memorije nakon izvrsenja programa
    vsm.PrintMemoryStatus();

    return 0;
}

