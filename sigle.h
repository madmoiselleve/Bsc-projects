#ifndef SIGLE_H
#define SIGLE_H

#include <vector>
#include "chaine.h"

class Sigle : public Chaine
{
public:
    Sigle();
    Sigle(const Sigle& other);
    // grace au mot cle virtual, le destructeur de la classe mere Chaine est egalement appele
    virtual ~Sigle();

    Sigle& operator=(const Sigle& other);
    friend Sigle operator+(const Sigle& first, const Sigle& second);
    char const& operator[](int i) const;
    Chaine& operator()(int i, Chaine& other);

    void print();

protected:
    std::vector<Chaine> sigle;
};

Sigle operator+(const Sigle& first, const Sigle& second);

#endif // SIGLE_H
