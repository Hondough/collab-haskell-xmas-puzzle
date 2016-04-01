#define the rows and cols
rows = [
  [7,3,1,1,7],
  [1,1,2,2,1,1],
  [1,3,1,3,1,1,3,1],
  [1,3,1,1,6,1,3,1],
  [1,3,1,5,2,1,3,1],
  [1,1,2,1,1],
  [7,1,1,1,1,1,7],
  [3,3],
  [1,2,3,1,1,3,1,1,2],
  [1,1,3,2,1,1],
  [4,1,4,2,1,2],
  [1,1,1,1,1,4,1,3],
  [2,1,1,1,2,5],
  [3,2,2,6,3,1],
  [1,9,1,1,2,1],
  [2,1,2,2,3,1],
  [3,1,1,1,1,5,1],
  [1,2,2,5],
  [7,1,2,1,1,1,3],
  [1,1,2,1,2,2,1],
  [1,3,1,4,5,1],
  [1,3,1,3,10,2],
  [1,3,1,1,6,6],
  [1,1,2,1,1,2],
  [7,2,1,2,5]]
cols = [
  [7,2,1,1,7],
  [1,1,2,2,1,1],
  [1,3,1,3,1,3,1,3,1],
  [1,3,1,1,5,1,3,1],
  [1,3,1,1,4,1,3,1],
  [1,1,1,2,1,1],
  [7,1,1,1,1,1,7],
  [1,1,3],
  [2,1,2,1,8,2,1],
  [2,2,1,2,1,1,1,2],
  [1,7,3,2,1],
  [1,2,3,1,1,1,1,1],
  [4,1,1,2,6],
  [3,3,1,1,1,3,1],
  [1,2,5,2,2],
  [2,2,1,1,1,1,1,2,1],
  [1,3,3,2,1,8,1],
  [6,2,1],
  [7,1,4,1,1,3],
  [1,1,1,1,4],
  [1,3,1,3,7,1],
  [1,3,1,1,1,2,1,1,4],
  [1,3,1,4,3,3],
  [1,1,2,2,2,6,1],
  [7,1,3,2,1,1]]

partialSolvedRows = {
    3 : [0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0],
    8 : [0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,0,0,0,0,0],
    16 : [0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0],
    21 : [0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0]
}
partialSolvedCols = {
    3 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
    4 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
    6 : [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
    7 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    9 : [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
    10 : [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
    11 : [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
    12 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    13 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    14 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    15 : [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
    16 : [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
    18 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    20 : [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0],
    21 : [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
}


import time
import copy

def groupCombination(numTiles, rowSize):
    acc = []
    for i in xrange(0,rowSize):
        current = ([0]*i) + [1]*numTiles + [0]*(rowSize - (i+numTiles))
        if len(current) > rowSize:
            return acc
        acc.append(current)
    return acc

def groupCombination1(numTiles, rowSize):
    acc = []
    if len(numTiles) == 0:
        return []
    newNumTiles = []
    if len(numTiles) == 1:
        return groupCombination(numTiles[0], rowSize)
    currentNumTiles = numTiles[0]
    if len(numTiles) >= 2:
        newNumTiles = numTiles[1:]

    nextNumTiles = 0
    if len(numTiles) >= 1:
        nextNumTiles = numTiles[1]

    for i in xrange(0,rowSize):
        current = ([0]*i) + [1]*currentNumTiles
        leftSides = groupCombination1(newNumTiles, rowSize - (i+currentNumTiles+1))

        for ls in leftSides:
            whole = current + [0] + ls
            if len(whole) > rowSize:
                break
            acc.append(whole)
    return acc

def getRaw():
    startTime = time.time()
    rawRows = []
    rawCols = []
    for row in rows:
        rawRows.append(groupCombination1(row,25))
    for col in cols:
        rawCols.append(groupCombination1(col,25))

    endTime = time.time()
    print "total time: " + str(endTime - startTime)
    return rawRows, rawCols

def writeSolutionToFile(m):
    writeStr = ''
    for row in m:
        for col in row:
            writeStr += str(col)
        writeStr += '\n'
    writeStr += '\n\n'
    f = open('solutions.txt', 'a')
    f.write(writeStr)
    f.close()
def writeToFile(rawRows, rawCols):
    toRowFile = ''
    toColFile = ''
    for i in range(0,len(rawRows)):
        toRowFile += "row " + str(i) + "      " + str(rows[i]) + "\n"
        toColFile += "col " + str(i) + "      " + str(cols[i]) +  "\n"
        for rcombo in rawRows[i]:
            toRowFile += "\t" + str(rcombo) + "\n"
        for ccombo in rawCols[i]:
            toColFile += "\t" + str(ccombo) + "\n"

    f = open("rowOut.txt",'w')
    f.write(toRowFile)
    f.close()
    f = open("colOut.txt",'w')
    f.write(toColFile)
    f.close()

def getCount(possibleDict):
    count = 0
    for index in possibleDict:
        for possible in possibleDict[index]:
            count += 1
    return count

def getDicts(rawRows, rawCols):
    rowDict = {}
    colDict = {}
    for i in range(0,len(rawRows)):
        rowDict[i] = rawRows[i]
        colDict[i] = rawCols[i]
    return rowDict, colDict

def initSolutionMatrix(n):
    m = []
    for i in xrange(0,n):
        m.append([])
        for j in xrange(0,n):
            m[i].append(0)
    return m

def addRowToMatrix(m,rowNum,row):
    m[rowNum] = row
    #print "adding row: " + str(row)
    #printSolutionMatrix(m)
    return m

def addColToMatrix(m,colNum,col):
    n = len(m)
    for i in xrange(0,n):
        m[i][colNum] = col[i]
    #print "adding col: " + str(col)
    #printSolutionMatrix(m)
    return m

def addSingleSolutionsToMatrix(m, solutionsDict, solutionsLenDict, sortedSolutionsLenCount, isRow):
    if(sortedSolutionsLenCount[0] == 1):
        for index in solutionsLenDict[1]:
            if isRow:
                m = addRowToMatrix(m, index, solutionsDict[index][0])
            else:
                m = addColToMatrix(m, index, solutionsDict[index][0])
            del solutionsDict[index]
        del solutionsLenDict[1]
        sortedSolutionsLenCount.pop(0)
    return m, solutionsDict, solutionsLenDict, sortedSolutionsLenCount

def sortPossibleSolutions(rowColDict):
    lenDict = {}
    for index in rowColDict:
        rowColLen = len(rowColDict[index])
        temp = lenDict.get(rowColLen,[])
        temp.append(index)
        lenDict[rowColLen] = temp

    rowColLens = []
    for numPossibleSolutions in lenDict:
        rowColLens.append(numPossibleSolutions)

    return lenDict, sorted(rowColLens)

def printSolutionMatrix(m):
    for row in m:
        print row
    print ''

def isSolved(rowsSolutionsDict, colsSolutionsDict):
    #print rowsSolutionsDict, colsSolutionsDict
    return len(rowsSolutionsDict) == 0 and len(colsSolutionsDict) == 0

def rowCanWork(mRow, row):
    for i in xrange(0,len(row)):
        if mRow[i] == 1 and row[i] != 1:
            return False
    return True

def colCanWork(n, m,colIndex, col):
    for i in xrange(0,n):
        if m[i][colIndex] == 1 and col[i] != 1:
            return False
    return True

def filterByPartials(partialDict, possibleDict):
    counter = 0
    for index in partialDict:
        possibleList = possibleDict.get(index, [])
        if len(possibleList) == 0:
            continue
        currentPartial = partialDict[index]
        for i in xrange(0,len(currentPartial)):
            for j in xrange(len(possibleList)-1, -1,-1):
                if currentPartial[i] == 1 and possibleList[j][i] != 1:
                    #print "item filtered! " + str()
                    counter += 1
                    possibleList.pop(j)
        possibleDict[index] = possibleList
    print "\tfiltered " + str(counter) + " items"
    return possibleDict

def filterByMatrix(m, rowsDict, colsDict):
    deleteLater = []
    for index in rowsDict:
        for i in xrange(len(rowsDict[index])-1, -1, -1):
            if not rowCanWork(m[index], rowsDict[index][i]):
                rowsDict[index].pop(i)
        if len(rowsDict[index]) == 1:
            m = addRowToMatrix(m,index,rowsDict[index][0])
            deleteLater.append(index)
    for i in deleteLater:
        del colsDict[i]
    deleteLater = []
    for index in colsDict:
        for i in xrange(len(colsDict[index])-1, -1, -1):
            if not colCanWork(len(m), m,index, colsDict[index][i]):
                colsDict[index].pop(i)
        if len(colsDict[index]) == 1:
            m = addColToMatrix(m,index,colsDict[index][0])
            deleteLater.append(index)
    for i in deleteLater:
        del colsDict[i]
    return m, rowsDict, colsDict

def solve3(mO, rowsDictO, colsDictO, rowsLenDictO, sortedRowLenKeysO, colsLenDictO, sortedColLenKeysO):
    m = copy.deepcopy(mO)
    rowsDict = copy.deepcopy(rowsDictO)
    colsDict = copy.deepcopy(colsDictO)
    rowsLenDict = copy.deepcopy(rowsLenDictO)
    sortedRowLenKeys = copy.deepcopy(sortedRowLenKeysO)
    colsLenDict = copy.deepcopy(colsLenDictO)
    sortedColLenKeys = copy.deepcopy(sortedColLenKeysO)

    if isSolved(rowsDict, colsDict):
        print "found solution!"
        print m
        writeSolutionToFile(m)
        return
    print "Rows left: " + str(getCount(rowsDict))
    print "Cols left: " + str(getCount(colsDict)) + '\n'
    rowIndexList = []
    colIndexList = []
    if len(sortedRowLenKeys) > 0:
        rowIndexList = rowsLenDict[sortedRowLenKeys[0]]
    if len(sortedColLenKeys) > 0:
        colIndexList = colsLenDict[sortedColLenKeys[0]]

    if len(rowIndexList) > 0 and len(colIndexList) > 0:
        for rowIndex in rowIndexList:
            for colIndex in colIndexList:
                for row in rowsDict[rowIndex]:
                    newRowsDict = copy.deepcopy(rowsDict)
                    newRowsDict[rowIndex] = [row]
                    newRowLenDict, newRowLenList = sortPossibleSolutions(newRowsDict)
                    newM = copy.deepcopy(m)
                    newM, newRowsDict, newRowLenDict, newRowLenList = addSingleSolutionsToMatrix(newM, newRowsDict, newRowLenDict, newRowLenList, True)
                    for col in colsDict[colIndex]:
                        newColsDict = copy.deepcopy(colsDict)
                        newColsDict[colIndex] = [col]
                        newColLenDict, newColLenList = sortPossibleSolutions(newColsDict)
                        newNewM = copy.deepcopy(newM)
                        newNewM, newColsDict, newColLenDict, newColLenList = addSingleSolutionsToMatrix(newNewM, newColsDict, newColLenDict, newColLenList, False)
                        newNewM,newRowsDict,newColsDict = filterByMatrix(newNewM, newRowsDict, newColsDict)
                        solve3(newNewM, newRowsDict, newColsDict, newRowLenDict, newRowLenList, newColLenDict, newColLenList)
    elif len(rowIndexList) > 0 and len(colIndexList) <= 0:
        for rowIndex in rowIndexList:
            for row in rowsDict[rowIndex]:
                newRowsDict = copy.deepcopy(rowsDict)
                newRowsDict[rowIndex] = [row]
                newRowLenDict, newRowLenList = sortPossibleSolutions(newRowsDict)
                newM = copy.deepcopy(m)
                newM, newRowsDict, newRowLenDict, newRowLenList = addSingleSolutionsToMatrix(newM, newRowsDict, newRowLenDict, newRowLenList, True)
                newM,newRowsDict,colsDict = filterByMatrix(newM, newRowsDict, colsDict)
                solve3(newM, newRowsDict, colsDict, newRowLenDict, newRowLenList, colsLenDict, sortedColLenKeys)
    elif len(rowIndexList) <= 0 and len(colIndexList) > 0:
        for colIndex in colIndexList:
            for col in colsDict[colIndex]:
                newColsDict = copy.deepcopy(colsDict)
                newColsDict[colIndex] = [col]
                newColLenDict, newColLenList = sortPossibleSolutions(newColsDict)
                newM = copy.deepcopy(newM)
                newM, newColsDict, newColLenDict, newColLenList = addSingleSolutionsToMatrix(newM, newColsDict, newColLenDict, newColLenList, False)
                newM,newRowsDict,colsDict = filterByMatrix(newM, rowsDict, newColsDict)
                solve3(newM, rowsDict, newColsDict, rowsLenDict, sortedRowLenKeys, newColLenDict, newColLenList)


def solve2(m, rowsDict, colsDict, rowsLenDict, sortedRowLenKeys, colsLenDict, sortedColLenKeys):
    if isSolved(rowsDict, colsDict):
        print "found solution!"
        print m
        return

    rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
    colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)

    m, rowsDict, rowsLenDict, sortedRowLenKeys = addSingleSolutionsToMatrix(m, rowsDict, rowsLenDict, sortedRowLenKeys, True)
    m, colsDict, colsLenDict, sortedColLenKeys = addSingleSolutionsToMatrix(m, colsDict, colsLenDict, sortedColLenKeys, False)

    #print "filtering Rows by hints! " + str(getCount(rowsDict))
    rowsDict = filterByPartials(partialSolvedRows, rowsDict)
    #print "Rows left: " + str(getCount(rowsDict)) + '\n'

    #print "filtering Cols by hints! " + str(getCount(colsDict))
    colsDict = filterByPartials(partialSolvedCols, colsDict)
    #print "Cols left: " + str(getCount(colsDict)) + '\n'

    #print "filtering by solution matrix!", "rows:", str(getCount(rowsDict)), "cols:",str(getCount(colsDict))
    m,rowsDict,colsDict = filterByMatrix(m,rowsDict,colsDict)
    #print "Rows left: " + str(getCount(rowsDict))
    #print "Cols left: " + str(getCount(colsDict)) + '\n'



    print "Rows left: " + str(getCount(rowsDict))
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    if len(sortedRowLenKeys) > 0:
        lenDict = rowsLenDict[sortedRowLenKeys.pop(0)]
        for index in lenDict:
            for row in rowsDict[index]:
                rowsDict2 = copy.deepcopy(rowsDict)
                rowsDict2[index] = [row]
                #print rowsDict2[index]
                solve2(copy.deepcopy(m), rowsDict2, copy.deepcopy(colsDict), copy.deepcopy(rowsLenDict), copy.deepcopy(sortedRowLenKeys), copy.deepcopy(colsLenDict), copy.deepcopy(sortedColLenKeys))
    if len(sortedColLenKeys) > 0:
        lenDict = colsLenDict[sortedColLenKeys.pop(0)]
        for index in lenDict:
            for col in colsDict[index]:
                colsDict2 = copy.deepcopy(colsDict)
                colsDict2[index] = [col]
                solve2(copy.deepcopy(m), copy.deepcopy(rowsDict), colsDict2, copy.deepcopy(rowsLenDict), copy.deepcopy(sortedRowLenKeys), copy.deepcopy(colsLenDict), copy.deepcopy(sortedColLenKeys))



def solve(m, rowsDict, colsDict, rowsLenDict, sortedRowLenKeys, colsLenDict, sortedColLenKeys):

    print "filtering Rows by hints! " + str(getCount(rowsDict))
    rowsDict = filterByPartials(partialSolvedRows, rowsDict)
    print "Rows left: " + str(getCount(rowsDict)) + '\n'

    print "filtering Cols by hints! " + str(getCount(colsDict))
    colsDict = filterByPartials(partialSolvedCols, colsDict)
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    print "filtering by solution matrix!", "rows:", str(getCount(rowsDict)), "cols:",str(getCount(colsDict))
    m,rowsDict,colsDict = filterByMatrix(m,rowsDict,colsDict)
    print "Rows left: " + str(getCount(rowsDict))
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
    colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)

    m, rowsDict, rowsLenDict, sortedRowLenKeys = addSingleSolutionsToMatrix(m, rowsDict, rowsLenDict, sortedRowLenKeys, True)
    m, colsDict, colsLenDict, sortedColLenKeys = addSingleSolutionsToMatrix(m, colsDict, colsLenDict, sortedColLenKeys, False)

    print "Rows left: " + str(getCount(rowsDict))
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    print sortedRowLenKeys, sortedColLenKeys
    print rowsLenDict[sortedRowLenKeys[0]]
    print "starting to solve!"

    prevRowsCount = getCount(rowsDict)
    prevColsCount = getCount(colsDict)
    retryCounter = 0

    while getCount(colsDict) != 0 and getCount(rowsDict) != 0:

        if(prevRowsCount == getCount(rowsDict) or prevColsCount == getCount(colsDict)):
            retryCounter += 1
        else:
            retryCounter = 0
            prevRowsCount = getCount(rowsDict)
            prevColsCount = getCount(colsDict)

        if retryCounter >= 5:
            print 'looks like we are stuck! breaking now!!!'
            break

        rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
        colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)

        m, rowsDict, rowsLenDict, sortedRowLenKeys = addSingleSolutionsToMatrix(m, rowsDict, rowsLenDict, sortedRowLenKeys, True)
        m, colsDict, colsLenDict, sortedColLenKeys = addSingleSolutionsToMatrix(m, colsDict, colsLenDict, sortedColLenKeys, False)

        #print sortedRowLenKeys, '\n' ,sortedColLenKeys

        print "Rows left: " + str(getCount(rowsDict))
        print "Cols left: " + str(getCount(colsDict))

        #print "working on rows\n"


    printSolutionMatrix(m)

def appRun():
    solutionMatrix = initSolutionMatrix(25)
    rawRows, rawCols = getRaw()
    rowsDict, colsDict = getDicts(rawRows,rawCols)
    rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
    colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)

    print len(rowsDict), '\n',len(colsDict)

    print "filtering Rows by hints! " + str(getCount(rowsDict))
    rowsDict = filterByPartials(partialSolvedRows, rowsDict)
    print "Rows left: " + str(getCount(rowsDict)) + '\n'

    print "filtering Cols by hints! " + str(getCount(colsDict))
    colsDict = filterByPartials(partialSolvedCols, colsDict)
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    print "filtering by solution matrix!", "rows:", str(getCount(rowsDict)), "cols:",str(getCount(colsDict))
    solutionMatrix,rowsDict,colsDict = filterByMatrix(solutionMatrix,rowsDict,colsDict)
    print "Rows left: " + str(getCount(rowsDict))
    print "Cols left: " + str(getCount(colsDict)) + '\n'

    rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
    colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)
    print len(rowsDict), '\n',len(colsDict)

    solutionMatrix, rowsDict, rowsLenDict, sortedRowLenKeys = addSingleSolutionsToMatrix(solutionMatrix, rowsDict, rowsLenDict, sortedRowLenKeys, True)
    solutionMatrix, colsDict, colsLenDict, sortedColLenKeys = addSingleSolutionsToMatrix(solutionMatrix, colsDict, colsLenDict, sortedColLenKeys, False)

    rowsLenDict, sortedRowLenKeys = sortPossibleSolutions(rowsDict)
    colsLenDict, sortedColLenKeys = sortPossibleSolutions(colsDict)

    solve3(solutionMatrix, rowsDict, colsDict, rowsLenDict, sortedRowLenKeys, colsLenDict, sortedColLenKeys)

appRun()
