<?xml version="1.0" encoding="utf-8"?>
<FlipProject flipVersion="4.3.3" location="C:\qq\Rpackages\flipPlots\Plots.Q" dataFileLocation="C:\qq\Rpackages\flipDataPreparation\inst\extdata\Cola Tracking - January to December.sav" dataFileDateTime="2014-12-23 07:12:00Z" dataFileSize="262436" grokVersion="8.61" saveVersion="1.34">
  <StripHtmlFromLabels value="undecided" />
  <Guid>cae059c8-b357-4b86-8d25-715de49122f4</Guid>
  <Options proportionsBessel="true" />
  <ReportOptions>
    <StyleScreen>47900440-6877-4e53-803f-87015d0abb79</StyleScreen>
    <StylePrint>b9d1bf41-02e8-48d9-a06c-03e224050a86</StylePrint>
    <StylePdf>b9d1bf41-02e8-48d9-a06c-03e224050a86</StylePdf>
    <StyleCopyPaste>b9d1bf41-02e8-48d9-a06c-03e224050a86</StyleCopyPaste>
  </ReportOptions>
  <OutputOptions />
  <ConditionalFormatsManager>
    <All>
      <BuiltIn formatName="HeatMap" guid="e86182c9-f84c-4f11-90ef-e14a8fe7b986" lastOutputColor="#ffd3b7" lastOutputColor2="#ff6600" position="0">
        <Parameters>
          <Parameter key="colorFrom">255, 211, 183</Parameter>
          <Parameter key="colorTo">255, 102, 0</Parameter>
          <Parameter key="style">2-Color Scale</Parameter>
        </Parameters>
        <LastSummary>Shade cells according to %</LastSummary>
      </BuiltIn>
    </All>
  </ConditionalFormatsManager>
  <rInstalledPackages>beeswarm,corrgram,devtools,ellipse,entropy,FactoMineR,ggmap,Hmisc,lme4,magrittr,mapdata,maps,plyr,RcmdrPlugin.KMggplot2,RColorBrewer,rmeta,ROAuth,rworldmap,scales,stringr,tm,WDI</rInstalledPackages>
  <QuestionSetExtras qset="75d4318f-9299-42d9-ac11-f017575c53d5" autoDetectQuestions="true" />
  <ImportReader qset="75d4318f-9299-42d9-ac11-f017575c53d5" type="SPSSFileReader" defaultEncoding="Windows-1252" dataFileLocation="C:\qq\Rpackages\flipDataPreparation\inst\extdata\Cola Tracking - January to December.sav" dataFileDateTime="2014-12-23 07:12:00Z" dataFileSize="262436" />
  <Variables qset="75d4318f-9299-42d9-ac11-f017575c53d5">
    <BinaryVariable change="43">
      <Name>under40</Name>
      <DefaultLabel />
      <Label>Under 40</Label>
      <Values type="Numeric">
        <Value missing="false">
          <Default>0</Default>
          <DefaultLabel>Not selected</DefaultLabel>
          <CurrentLabel>40 or more</CurrentLabel>
        </Value>
        <Value missing="false">
          <Default>1</Default>
          <DefaultLabel>Selected</DefaultLabel>
          <CurrentLabel>Under 40</CurrentLabel>
        </Value>
      </Values>
      <Tree>
        <Term op="Any of" question="9d2d99a2-9e25-421a-bc72-63a7499c5c7e" type="OtherCategorical">
          <NumValue>21</NumValue>
          <NumValue>27</NumValue>
          <NumValue>32</NumValue>
          <NumValue>37</NumValue>
        </Term>
      </Tree>
    </BinaryVariable>
    <Variable change="17" qid="24a83ca8-91b7-4b2c-9aed-9340bc9537b9">
      <Name>UniqueID</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17" qid="79d8ac21-7062-4083-b717-59618ef33a42">
      <Name>date</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>d1</Name>
      <DefaultLabel>Age</DefaultLabel>
      <Values type="Numeric">
        <Value missing="false">
          <Default>21</Default>
          <DefaultLabel>18 to 24</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>27</Default>
          <DefaultLabel>25 to 29</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>32</Default>
          <DefaultLabel>30 to 34</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>37</Default>
          <DefaultLabel>35 to 39</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>42</Default>
          <DefaultLabel>40 to 44</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>47</Default>
          <DefaultLabel>45 to 49</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>52</Default>
          <DefaultLabel>50 to 54</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>60</Default>
          <DefaultLabel>55 to 64</DefaultLabel>
        </Value>
        <Value missing="false">
          <Default>77</Default>
          <DefaultLabel>65 or more</DefaultLabel>
        </Value>
      </Values>
    </Variable>
    <Variable change="1" qid="e1f860cc-a7c2-4f6b-980d-ad80945a6d95">
      <Name>d2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1" qid="6c3377ed-95f1-49d9-a8f6-43b6cb9dc61b">
      <Name>d3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1" qid="771882aa-197a-44db-93df-a72bf74341b6">
      <Name>d4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1" qid="9dc831cd-d30c-4bdb-b8c5-1cb94757059a">
      <Name>d4o</Name>
      <Values type="Text" />
    </Variable>
    <Variable change="17" qid="f4bf8388-5f8a-4077-80e6-e527dbf9bfe7">
      <Name>q1a_1</Name>
      <Values type="Text" />
    </Variable>
    <Variable change="17" qid="82f526bf-137d-46d3-a7cb-46900556be71">
      <Name>q1a_b</Name>
      <Values type="Text" />
    </Variable>
    <Variable change="17" qid="90f6985c-83ce-45a1-8b86-f21a7867d54c">
      <Name>q1a_c</Name>
      <Values type="Text" />
    </Variable>
    <Variable change="17">
      <Name>q1b_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q1b_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q1b_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q1b_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q1b_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q1b_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2a_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q2b_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1" qid="b41f0b2c-d081-436d-89cf-8dbeab8ea09c">
      <Name>q3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4a</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4b</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4c</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4d</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4e</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1">
      <Name>q4f</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5a7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5b7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5c7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5d7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5e7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5f7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5g7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5h7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q5i7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17" qid="9a70f57f-e154-4047-aa18-11bf590c38f4">
      <Name>q6oe</Name>
      <Values type="Text" />
    </Variable>
    <Variable change="1">
      <Name>q7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="1" qid="9f7a2d7c-be96-4ded-8c26-80c21da449a7">
      <Name>q8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_6_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_5_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_4_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_3_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_2_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_1_0</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_8</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_7</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_6</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_5</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_4</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_3</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_2</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_1</Name>
      <Values type="Numeric" />
    </Variable>
    <Variable change="17">
      <Name>q9_0_0</Name>
      <Values type="Numeric" />
    </Variable>
  </Variables>
  <Questions qset="75d4318f-9299-42d9-ac11-f017575c53d5">
    <Question change="43" type="Single Response - Categorical">
      <Name>Under 40</Name>
      <Ref>66687f09-8f9b-42fc-8d1d-bc654bf3a61a</Ref>
      <Variable>under40</Variable>
      <Tags>
        <Tag>Filter</Tag>
      </Tags>
    </Question>
    <Question change="43" type="Single Response - Categorical">
      <Name>Age</Name>
      <Ref>9d2d99a2-9e25-421a-bc72-63a7499c5c7e</Ref>
      <Variable>d1</Variable>
      <DataReduction>
        <OriginalLabels>
          <LabelPair>
            <CodeIndex>21</CodeIndex>
            <Label>18 to 24</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>27</CodeIndex>
            <Label>25 to 29</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>32</CodeIndex>
            <Label>30 to 34</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>37</CodeIndex>
            <Label>35 to 39</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>42</CodeIndex>
            <Label>40 to 44</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>47</CodeIndex>
            <Label>45 to 49</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>52</CodeIndex>
            <Label>50 to 54</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>60</CodeIndex>
            <Label>55 to 64</Label>
          </LabelPair>
          <LabelPair>
            <CodeIndex>77</CodeIndex>
            <Label>65 or more</Label>
          </LabelPair>
        </OriginalLabels>
        <CodeFrame>
          <Code ref="fb5b125b-5b47-471b-af52-cecaa2270c34">
            <Label>Under 40</Label>
            <Value>21</Value>
            <Value>27</Value>
            <Value>32</Value>
            <Value>37</Value>
          </Code>
          <Code ref="4b4f0176-fc66-4a20-9c95-0f1a46a0d32b">
            <Label>Over 40</Label>
            <Value>42</Value>
            <Value>47</Value>
            <Value>52</Value>
            <Value>60</Value>
            <Value>77</Value>
          </Code>
          <Code ref="4fd01560-6548-4f78-b8c6-997fd875b2b4">
            <Label>NET</Label>
            <Value>21</Value>
            <Value>27</Value>
            <Value>32</Value>
            <Value>37</Value>
            <Value>42</Value>
            <Value>47</Value>
            <Value>52</Value>
            <Value>60</Value>
            <Value>77</Value>
          </Code>
        </CodeFrame>
      </DataReduction>
    </Question>
    <Question change="17" type="Multiple Response - Binary" needCheckValuesToCount="true">
      <Name>Awareness</Name>
      <Ref>442fecd7-5b7f-4269-ae7e-99b68196b427</Ref>
      <Variable>q1b_1</Variable>
      <Variable>q1b_2</Variable>
      <Variable>q1b_3</Variable>
      <Variable>q1b_4</Variable>
      <Variable>q1b_5</Variable>
      <Variable>q1b_6</Variable>
    </Question>
    <Question change="17" type="Two Dimensional - Numeric">
      <Name>q2</Name>
      <Ref>0d1ddaeb-5e88-447e-a043-a7febaf81aa2</Ref>
      <Variable>q2a_1</Variable>
      <Variable>q2a_2</Variable>
      <Variable>q2a_3</Variable>
      <Variable>q2a_4</Variable>
      <Variable>q2a_5</Variable>
      <Variable>q2a_6</Variable>
      <Variable>q2b_1</Variable>
      <Variable>q2b_2</Variable>
      <Variable>q2b_3</Variable>
      <Variable>q2b_4</Variable>
      <Variable>q2b_5</Variable>
      <Variable>q2b_6</Variable>
    </Question>
    <Question change="1" type="Multiple Response - Categorical">
      <Name>Brand attitude</Name>
      <MultipleResponseSetName>$Brandattitude</MultipleResponseSetName>
      <Ref>c52bc993-ddc9-48f4-bacc-b7fecf9d06ed</Ref>
      <Variable>q4a</Variable>
      <Variable>q4b</Variable>
      <Variable>q4c</Variable>
      <Variable>q4d</Variable>
      <Variable>q4e</Variable>
      <Variable>q4f</Variable>
    </Question>
    <Question change="17" type="Two Dimensional - Binary" needCheckValuesToCount="true">
      <Name>q5</Name>
      <Ref>b0023fc4-53e3-439f-9041-2a6416404057</Ref>
      <Variable>q5a1</Variable>
      <Variable>q5a2</Variable>
      <Variable>q5a3</Variable>
      <Variable>q5a4</Variable>
      <Variable>q5a5</Variable>
      <Variable>q5a6</Variable>
      <Variable>q5a7</Variable>
      <Variable>q5b1</Variable>
      <Variable>q5b2</Variable>
      <Variable>q5b3</Variable>
      <Variable>q5b4</Variable>
      <Variable>q5b5</Variable>
      <Variable>q5b6</Variable>
      <Variable>q5b7</Variable>
      <Variable>q5c1</Variable>
      <Variable>q5c2</Variable>
      <Variable>q5c3</Variable>
      <Variable>q5c4</Variable>
      <Variable>q5c5</Variable>
      <Variable>q5c6</Variable>
      <Variable>q5c7</Variable>
      <Variable>q5d1</Variable>
      <Variable>q5d2</Variable>
      <Variable>q5d3</Variable>
      <Variable>q5d4</Variable>
      <Variable>q5d5</Variable>
      <Variable>q5d6</Variable>
      <Variable>q5d7</Variable>
      <Variable>q5e1</Variable>
      <Variable>q5e2</Variable>
      <Variable>q5e3</Variable>
      <Variable>q5e4</Variable>
      <Variable>q5e5</Variable>
      <Variable>q5e6</Variable>
      <Variable>q5e7</Variable>
      <Variable>q5f1</Variable>
      <Variable>q5f2</Variable>
      <Variable>q5f3</Variable>
      <Variable>q5f4</Variable>
      <Variable>q5f5</Variable>
      <Variable>q5f6</Variable>
      <Variable>q5f7</Variable>
      <Variable>q5g1</Variable>
      <Variable>q5g2</Variable>
      <Variable>q5g3</Variable>
      <Variable>q5g4</Variable>
      <Variable>q5g5</Variable>
      <Variable>q5g6</Variable>
      <Variable>q5g7</Variable>
      <Variable>q5h1</Variable>
      <Variable>q5h2</Variable>
      <Variable>q5h3</Variable>
      <Variable>q5h4</Variable>
      <Variable>q5h5</Variable>
      <Variable>q5h6</Variable>
      <Variable>q5h7</Variable>
      <Variable>q5i1</Variable>
      <Variable>q5i2</Variable>
      <Variable>q5i3</Variable>
      <Variable>q5i4</Variable>
      <Variable>q5i5</Variable>
      <Variable>q5i6</Variable>
      <Variable>q5i7</Variable>
    </Question>
    <Question change="1" type="Single Response - Categorical">
      <Name>Weight-consciousness</Name>
      <Ref>7a97f448-da99-4c4d-a1f2-48cb3c515100</Ref>
      <Variable>q7</Variable>
      <Tags>
        <Tag>Weight</Tag>
      </Tags>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I</Name>
      <Ref>5970a2cb-b793-4cc4-a6e3-e02357c866cd</Ref>
      <Variable>q9_6_8</Variable>
      <Variable>q9_6_7</Variable>
      <Variable>q9_6_6</Variable>
      <Variable>q9_6_5</Variable>
      <Variable>q9_6_4</Variable>
      <Variable>q9_6_3</Variable>
      <Variable>q9_6_2</Variable>
      <Variable>q9_6_1</Variable>
      <Variable>q9_6_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I1</Name>
      <Ref>ea00f541-27e1-4272-99e1-d62951ae5719</Ref>
      <Variable>q9_5_8</Variable>
      <Variable>q9_5_7</Variable>
      <Variable>q9_5_6</Variable>
      <Variable>q9_5_5</Variable>
      <Variable>q9_5_4</Variable>
      <Variable>q9_5_3</Variable>
      <Variable>q9_5_2</Variable>
      <Variable>q9_5_1</Variable>
      <Variable>q9_5_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I2</Name>
      <Ref>9c8112a4-30a7-42de-a534-79e12ac6777b</Ref>
      <Variable>q9_4_8</Variable>
      <Variable>q9_4_7</Variable>
      <Variable>q9_4_6</Variable>
      <Variable>q9_4_5</Variable>
      <Variable>q9_4_4</Variable>
      <Variable>q9_4_3</Variable>
      <Variable>q9_4_2</Variable>
      <Variable>q9_4_1</Variable>
      <Variable>q9_4_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I3</Name>
      <Ref>fc2b2d32-f1d1-4e7a-8784-700dfec7747a</Ref>
      <Variable>q9_3_8</Variable>
      <Variable>q9_3_7</Variable>
      <Variable>q9_3_6</Variable>
      <Variable>q9_3_5</Variable>
      <Variable>q9_3_4</Variable>
      <Variable>q9_3_3</Variable>
      <Variable>q9_3_2</Variable>
      <Variable>q9_3_1</Variable>
      <Variable>q9_3_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I4</Name>
      <Ref>f4eb66c7-63b0-41c4-8a62-eabc56c9297e</Ref>
      <Variable>q9_2_8</Variable>
      <Variable>q9_2_7</Variable>
      <Variable>q9_2_6</Variable>
      <Variable>q9_2_5</Variable>
      <Variable>q9_2_4</Variable>
      <Variable>q9_2_3</Variable>
      <Variable>q9_2_2</Variable>
      <Variable>q9_2_1</Variable>
      <Variable>q9_2_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I5</Name>
      <Ref>2b1e50b8-def6-4ca5-818b-339234486bce</Ref>
      <Variable>q9_1_8</Variable>
      <Variable>q9_1_7</Variable>
      <Variable>q9_1_6</Variable>
      <Variable>q9_1_5</Variable>
      <Variable>q9_1_4</Variable>
      <Variable>q9_1_3</Variable>
      <Variable>q9_1_2</Variable>
      <Variable>q9_1_1</Variable>
      <Variable>q9_1_0</Variable>
    </Question>
    <Question change="17" type="Multiple Response - Numeric">
      <Name>Max-Diff Experiment: I6</Name>
      <Ref>1453133a-cc10-469e-8735-593d78d022d8</Ref>
      <Variable>q9_0_8</Variable>
      <Variable>q9_0_7</Variable>
      <Variable>q9_0_6</Variable>
      <Variable>q9_0_5</Variable>
      <Variable>q9_0_4</Variable>
      <Variable>q9_0_3</Variable>
      <Variable>q9_0_2</Variable>
      <Variable>q9_0_1</Variable>
      <Variable>q9_0_0</Variable>
    </Question>
  </Questions>
  <Analyses>
    <Ref>74713e53-eda2-4f73-8ae1-e1a865227a46</Ref>
    <Table change="43">
      <Ref>888ffaed-a8e0-4860-9664-948a58d08ffc</Ref>
      <Name>Age</Name>
      <Show>9d2d99a2-9e25-421a-bc72-63a7499c5c7e</Show>
      <ByItem>SUMMARY</ByItem>
      <TableSeries>
        <Name>Percent</Name>
      </TableSeries>
    </Table>
    <Group>
      <Name>HTMLwidgets</Name>
      <Ref>e70eeec4-b85b-4dd6-b94b-dee2092db4be</Ref>
      <Group>
        <Name>dygraphs</Name>
        <Ref>2bbcb777-8b4d-4468-9750-0d4e174c975c</Ref>
      </Group>
    </Group>
    <Group>
      <Name>Treemap</Name>
      <Ref>0ef5934f-a942-46fb-8023-1e6b246c3a0c</Ref>
      <RItem width="966" height="576" splitV="0.45000000000000001" change="55">
        <Ref>285ae6cd-efd3-4316-bad0-1d87a12d66a4</Ref>
        <Name>obj45</Name>
        <Controls />
        <Code>library(treemap)
data(GNI2010)
tmPlot(GNI2010
, index = c("continent", "iso3")
, vSize = "population"
, vColor = "GNI"
, type = "value")</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <RItem timetaken="15657.3783" width="1053" height="677" splitV="0.45000000000000001" change="54">
      <Ref>9aa4ee50-2147-4762-bed9-47d7ead38c5c</Ref>
      <Name>BulletGraph</Name>
      <Controls />
      <Code>#'  gridBulletGraphH: Horizontale Bullet Graph
#'  
#'  @author Simon MÃ¼ller
#'  
#'  @param bgData:  data.frame with
#'                     measure  : label of what's being measured
#'                     unit     : label of units of the measure
#'                     high     : the high value for the measure
#'                     mean     : the mean value for the measure
#'                     low      : the low value for the measure
#'                     target   : the target value for the measure
#'                     value    : the actual value of the measure
#'                     
#'  @param nticks:  number of ticks. If single number all bullet graphs have the 
#'                  same number of ticks, else a vector of nrow bgData is needed
#'      
#'  @param format:  format of the ticks. Single or vector. Actually implemented:
#'                    s: standard (do nothing)
#'                    k: komma (decimal point, thousands komma)
#'                    d: point (vice verca to k)
#'                    p: add percent
#'                    
#'  @param bcol:    vector of colours
#'  
#'  @param tcol:    colour of the target bar
#'  
#'  @param vcol:    colour of the vertical bar
#'

library(grid)

gridBulletGraphH &lt;- function(bgData, nticks=3, format="s", bcol=c("red", "yellow", "green"), tcol="black", vcol="black", font=25, scfont=15) {
  
  # Data Prep
  n &lt;- nrow(bgData)
  nam &lt;- c("low", "mean", "high", "target", "value")
  datMat &lt;- as.matrix(bgData[, nam])
  
  # Nticks/Format Prep
  if (length(nticks) == 1) {
    nticks &lt;- rep(nticks, n)
  }
  if (length(format) == 1) {
    format &lt;- rep(format, n)
  }
  
  # Layout
  hl &lt;- rep(1, n + 2)
  hu &lt;- c("lines", rep("null", n), "lines")
  layout &lt;- grid.layout(n + 2, 4, widths = unit(c(1, 1, 5, 2), c("lines", "null", "null", "lines")),
                        heights = unit(hl, hu))
  
  # Set Layout
  grid.newpage()
  pushViewport(plotViewport(c(0, 0, 0, 0), layout = layout))
  for (i in 1:n) {
    #
    vp &lt;- viewport(layout.pos.row = i+1, 
                   layout.pos.col = 3)
    pushViewport(vp)
    
    # Sublayout
    subLayout &lt;- grid.layout(nrow    = 3, 
                             ncol    = 1, 
                             heights = unit(c(1, 2, 1), c("null", "null", "null")))
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    
    vp &lt;- viewport(layout.pos.row = 2, 
                   layout.pos.col = 1, 
                   xscale         = c(0, datMat[i, 3]))
    pushViewport(vp)
    
    # x-Axis Labels
    # Formatierung Label
    if (format[i] == "s") {
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
    } else if (format[i] == "p"){
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
      labels &lt;- paste0(labels, "%")
    } else if (format[i] == "k") {
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
      labels &lt;- format(labels, digits=10, nsmall=0, decimal.mark=".", big.mark=",")
    }
    
    grid.xaxis(at=brks, label=labels, gp=gpar(fontsize=scfont, col="black", fontface="bold"))
    
    grid.rect(x      = c(0, datMat[i, 1:2]) / datMat[i, 3], 
              width  = unit(diff(c(0, datMat[i, 1:3])), "native"),
              y      = rep(0.5, 3), 
              height = 1, 
              just   = "left",
              gp     = gpar(fill=bcol, col=bcol))
    
    grid.rect(x      = c(0, datMat[i, 5]),  
              width  = unit(diff(c(0, datMat[i, 5])), "native"),
              y      = 0.5, 
              height = 0.5, 
              gp     = gpar(fill=vcol, col=vcol), just="left")
    
    a &lt;- datMat[i, 3] * 0.005
    grid.rect(x      = datMat[i, 4] / datMat[i, 3],  
              width  = unit(a, "native"),
              y      = 0.5, 
              height = 0.8, 
              gp     = gpar(fill=tcol, col=tcol), just="left")
    
    upViewport(n=3)
    
    
    # Annotation
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=layout))
    vp &lt;- viewport(layout.pos.row = i+1, 
                   layout.pos.col = 2)
    pushViewport(vp)
    
    # Sublayout 1: Same layout as graph
    subLayout &lt;- grid.layout(nrow    = 3, 
                             ncol    = 1, 
                             heights = unit(c(1, 2, 1), c("null", "null", "null")))
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    vp &lt;- viewport(layout.pos.row = 2, 
                   layout.pos.col = 1)
    pushViewport(vp)
    
    # Sublayout 2: two rows of text; centred middle of graph
    subLayout &lt;- grid.layout(nrow    = 2, 
                             ncol    = 1, 
                             heights = unit(c(1, 1), c("null", "null")))
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    # First Text: Measure
    vp &lt;- viewport(layout.pos.row = 1, 
                   layout.pos.col = 1) 
    pushViewport(vp)
    grid.text(label = bgData$measure[i], 
              just  = "right", 
              gp    = gpar(fontsize=font, col="black", fontface="bold"), 
              x     = .9,
              y     = .5)
    upViewport()
    
    # Second Text: Unit
    vp &lt;- viewport(layout.pos.row = 2, 
                   layout.pos.col = 1) 
    pushViewport(vp)
    grid.text(label = bgData$units[i],
              just  = "right", 
              gp    = gpar(fontsize=scfont, col="black"), 
              x     = .9,
              y     = .5)
    upViewport(n=5)
  }
}

#'  gridBulletGraphV: Vertical Bullet Graph
#'  
#'  @author Simon MÃ¼ller
#'  
#'  @param bgData:  data.frame with
#'                     measure  : label of what's being measured
#'                     unit     : label of units of the measure
#'                     high     : the high value for the measure
#'                     mean     : the mean value for the measure
#'                     low      : the low value for the measure
#'                     target   : the target value for the measure
#'                     value    : the actual value of the measure
#'                     
#'  @param nticks:  number of ticks. If single number all bullet graphs have the 
#'                  same number of ticks, else a vector of nrow bgData is needed
#'      
#'  @param format:  format of the ticks. Single or vector. Actually implemented:
#'                    s: standard (do nothing)
#'                    k: komma (decimal point, thousands komma)
#'                    d: point (vice verca to k)
#'                    p: add percent
#'                    
#'  @param bcol:    vector of colours
#'  
#'  @param tcol:    colour of the target bar
#'  
#'  @param vcol:    colour of the vertical bar
#'

gridBulletGraphV &lt;- function(bgData, nticks=3, format="s", bcol=c("red", "yellow", "green"), tcol="black", vcol="black", font=25, scfont=15) {
  
  # Data Prep
  n &lt;- nrow(bgData)
  nam &lt;- c("low", "mean", "high", "target", "value")
  datMat &lt;- as.matrix(bgData[, nam])
  
  # Nticks/Format Prep
  if (length(nticks) == 1) {
    nticks &lt;- rep(nticks, n)
  }
  if (length(format) == 1) {
    format &lt;- rep(format, n)
  }
  
  # Layout
  hl &lt;- rep(1, n + 2)
  hu &lt;- c("lines", rep("null", n), "lines")
  layout &lt;- grid.layout(4, n + 2, widths = unit(hl, hu),
                        heights = unit(c(1, 1, 5, 2), c("lines", "null", "null", "lines")))
  
  # Set Layout
  grid.newpage()
  pushViewport(plotViewport(c(0, 0, 0, 0), layout = layout))
  for (i in 1:n) {
    #
    vp &lt;- viewport(layout.pos.row = 3, 
                   layout.pos.col = i+1)
    pushViewport(vp)
    
    # Sublayout
    subLayout &lt;- grid.layout(nrow    = 1, 
                             widths  = unit(c(1, 2, 1), c("null", "null", "null")),
                             ncol    = 3) 
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    
    vp &lt;- viewport(layout.pos.row = 1, 
                   layout.pos.col = 2, 
                   yscale         = c(0, datMat[i, 3]))
    pushViewport(vp)
    
    # x-Axis Labels
    # Formatierung Label
    if (format[i] == "s") {
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
    } else if (format[i] == "p"){
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
      labels &lt;- paste0(labels, "%")
    } else if (format[i] == "k") {
      brks &lt;- labels &lt;- round(seq(0, datMat[i, 3], length=nticks[i]), 0)
      labels &lt;- format(labels, digits=10, nsmall=0, decimal.mark=".", big.mark=",")
    }
    
    grid.yaxis(at=brks, label=labels, gp=gpar(fontsize=scfont, col="black", fontface="bold"))
    
    grid.rect(y      = c(0, datMat[i, 1:2]) / datMat[i, 3], 
              height = unit(diff(c(0, datMat[i, 1:3])), "native"),
              x      = rep(0.5, 3), 
              width  = 1, 
              just   = "bottom",
              gp     = gpar(fill=bcol, col=bcol))
    
    grid.rect(y      = c(0, datMat[i, 5]),  
              height = unit(diff(c(0, datMat[i, 5])), "native"),
              x      = 0.5, 
              width  = 0.5, 
              gp     = gpar(fill=vcol, col=vcol), just="bottom")
    
    a &lt;- datMat[i, 1] * 0.01
    grid.rect(y      = datMat[i, 4] / datMat[i, 3],  
              height = unit(a, "native"),
              x      = 0.5, 
              width  = 0.8, 
              gp     = gpar(fill=tcol, col=tcol), just="bottom")
    
    upViewport(n=3)
    
    
    # Annotation
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=layout))
    vp &lt;- viewport(layout.pos.row = 2, 
                   layout.pos.col = i+1)
    pushViewport(vp)
    
    # Sublayout 1: Same layout as graph
    subLayout &lt;- grid.layout(nrow    = 1, 
                             ncol    = 3, 
                             widths  = unit(c(1, 2, 1), c("null", "null", "null")))
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    vp &lt;- viewport(layout.pos.row = 1, 
                   layout.pos.col = 2)
    pushViewport(vp)
    
    # Sublayout 2: two rows of text; centred middle of graph
    subLayout &lt;- grid.layout(nrow    = 2, 
                             ncol    = 1, 
                             widths = unit(c(1, 1), c("null", "null")))
    pushViewport(plotViewport(c(0, 0, 0, 0), layout=subLayout))
    
    # First Text: Measure
    vp &lt;- viewport(layout.pos.row = 1, 
                   layout.pos.col = 1) 
    pushViewport(vp)
    grid.text(label = bgData$measure[i], 
              just  = "bottom", 
              gp    = gpar(fontsize=font, col="black", fontface="bold"), 
              x     = .5,
              y     = 0.1)
    upViewport()
    
    # Second Text: Unit
    vp &lt;- viewport(layout.pos.row = 2, 
                   layout.pos.col = 1) 
    pushViewport(vp)
    grid.text(label = bgData$units[i],
              just  = "bottom", 
              gp    = gpar(fontsize=scfont, col="black"), 
              x     = .5,
              y     = .5)
    upViewport(n=5)
  }
}

ytd2005 &lt;- data.frame(
    measure=c("Revenue", "Profit", "Avg Order Size", "New Customers", "Cust Satisfaction"),
    units=c("U.S. $ (1,000s)", "%", "U.S. $", "Count", "Top Rating of 5"),
    low=c(150, 20, 350, 1400, 3.5), 
    mean=c(225, 25, 500, 2000, 4.25),
    high=c(300, 30, 600, 2500, 5),
    target=c(250, 26, 550, 2100, 4.2),
    value=c(275, 22.5, 310, 1700, 4.5)
  )
nticks &lt;- c(7, 7, 7, 6, 7)
format &lt;- c("s", "p", "s", "k", "s")
col1 &lt;- c("#a5a7a9", "#c5c6c8", "#e6e6e7")
BulletGraph = gridBulletGraphH(ytd2005, nticks=nticks, format=format, bcol=col1)
BulletGraph</Code>
      <Arguments />
      <Referenced />
      <Version>0</Version>
    </RItem>
    <Group>
      <Name>Twiiter</Name>
      <Ref>3bfac0f9-2f42-459b-b9c4-8128cb0642ae</Ref>
      <TextItem change="40">
        <Ref>7c4fec88-70b1-4a9d-9abd-c1fff9664b4e</Ref>
        <Name>Title</Name>
        <Html />
        <TitleHtml />
        <CreatedOnWeb>false</CreatedOnWeb>
      </TextItem>
      <RItem splitV="0.45000000000000001" change="55">
        <Ref>144e01f2-9c59-42b0-b053-a8aaa3fe9977</Ref>
        <Name>obj24</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="55">
        <Ref>b08568a6-828e-4434-b132-802ab5a13df1</Ref>
        <Name>obj8</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="39">
        <Ref>c591244a-b9d5-4ce9-9b2f-8e7a3e2918c8</Ref>
        <Name>obj18</Name>
        <Controls />
        <Code> download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Text analytics tools</Name>
      <Ref>aeb4e0b0-deef-457c-913b-df03d8aad34c</Ref>
      <RItem width="1053" height="592" splitV="0.45000000000000001" change="52">
        <Ref>bffba0f5-5820-4726-8793-fc1d9afc7516</Ref>
        <Name>obj5</Name>
        <Controls />
        <Code># Cleaning the text
y = q6oe
x = under40#`Under 40`#d3
stop.words = c("Diet Coke","Coca-Cola","Coke Zero","Diet Pepsi","Pepsi Lite","Pepsi Max", "Coke","Pepsi")
                           
# cleaning the text                           
#y = clean.text(y)
y = tolower(y)                           
dodgy.data = is.na(x) | y == ""
x = factor(x[!dodgy.data])
levs = levels(x)
lev1 = levs[1]                           
lev2 = levs[2]                           
y = y[!dodgy.data]

#dat = c(paste, collapse=" "),paste(], collapse=" "))                           
      

# get text
obama_txt = y[x == levs[1]]
romney_txt = y[x == levs[2]]


## Step 3: Create function to clean text
# clean text function
clean.text &lt;- function(some_txt) {
   some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
   some_txt = gsub("@\\w+", "", some_txt)
   some_txt = gsub("[[:punct:]]", "", some_txt)
   some_txt = gsub("[[:digit:]]", "", some_txt)
   some_txt = gsub("http\\w+", "", some_txt)
   some_txt = gsub("[ \t]{2,}", "", some_txt)
   some_txt = gsub("^\\s+|\\s+$", "", some_txt)

   # define "tolower error handling" function
   try.tolower = function(x)
   {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
      y = tolower(x)
      return(y)
   }

   some_txt = sapply(some_txt, try.tolower)
   some_txt = some_txt[some_txt != ""]
   names(some_txt) = NULL
   return(some_txt)
}


## Step 4: Let's clean the text
# clean text
obama_clean = clean.text(obama_txt)
romney_clean = clean.text(romney_txt)

# join cleaned texts in a single vector
obamas = paste(obama_clean, collapse=" ")
romneys = paste(romney_clean, collapse=" ")
oba_rom = c(obamas, romneys)


## Step 5: Create corpus and term-document matrix
# Corpus
or_corpus = Corpus(VectorSource(oba_rom))

# remove stopwords
skipwords = c(stopwords("english"), "president", "presidents","obama", "obamas", "video", "todays", "reads", "live", "watch")
or_corpus = tm_map(or_corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(or_corpus)


## Step 6: Create data frames from term-document matrix
# create data frame
or_df = as.data.frame(inspect(tdm))
names(or_df) = c("obama.txt", "romney.txt")

# get rid of low frequency words
or_df = subset(or_df, obama.txt&gt;2 &amp; romney.txt&gt;2)

# calculate frequency differences
or_df$freq.dif = or_df$obama.txt - or_df$romney.txt

# twitted more often by Obama
obama_df = subset(or_df, freq.dif &gt; 0)

# twitted more often by Romney
romney_df = subset(or_df, freq.dif &lt; 0)

# twitted equally
both_df = subset(or_df, freq.dif == 0)


## Step 7: Create function to get the words spacing for the plot
# function
optimal.spacing &lt;- function(spaces)
{
   if(spaces &gt; 1) {
      spacing &lt;- 1 / spaces
      if(spaces%%2 &gt; 0) {
         lim = spacing * floor(spaces/2)
         return(seq(-lim, lim, spacing))
      }
      else {
         lim = spacing * (spaces-1)
         return(seq(-lim, lim, spacing*2))
      }
   }
   else {
      # add some jitter when 0
      return(jitter(0, amount=0.2))
   }
}


## Step 8: Apply function optimal.spacing
# Get spacing for each frequency type
obama_spacing = sapply(table(obama_df$freq.dif),
function(x) optimal.spacing(x))

romney_spacing = sapply(table(romney_df$freq.dif),
function(x) optimal.spacing(x))

both_spacing = sapply(table(both_df$freq.dif),
function(x) optimal.spacing(x))


## Step 9: Add spacing column to data frames
# add spacings
obama_optim = rep(0, nrow(obama_df))
for(n in names(obama_spacing)) {
   obama_optim[obama_df$freq.dif == as.numeric(n)] &lt;- obama_spacing[[n]]
}
obama_df = transform(obama_df, Spacing=obama_optim)
romney_optim = rep(0, nrow(romney_df))
for(n in names(romney_spacing)) {
   romney_optim[romney_df$freq.dif == as.numeric(n)] &lt;- romney_spacing[[n]]
}
romney_df = transform(romney_df, Spacing=romney_optim)

both_df$Spacing = as.vector(both_spacing)


## Step 10: Let's visualize the wordcloud
# use ggplot
labs = row.names(obama_df)
obama_df
ggplot(obama_df, aes(x=freq.dif, y=Spacing)) +
geom_text(aes(size=obama.txt, label=labs,
    colour=freq.dif), alpha=0.7, family='Times')# +
# geom_text(data=romney_df, aes(x=freq.dif, y=Spacing,
#    label=row.names(romney_df), size=romney.txt, color=freq.dif),
#    alpha=0.7, family='Times') +
# geom_text(data=both_df, aes(x=freq.dif, y=Spacing,
#    label=row.names(both_df), size=obama.txt, color=freq.dif),
#    alpha=0.7, family='Times') #+
# # scale_size(range=c(3,11)) +
# scale_colour_gradient(low="red3", high="blue3", guide="none") +
# scale_x_continuous(breaks=c(min(romney_df$freq.dif), 0, max(obama_df$freq.dif)),
# labels=c("Twitted More by Romney","Twitted Equally","Twitted More by Obama")) +
# scale_y_continuous(breaks=c(0), labels=c("")) +
# labs(x="", y="", size="Word Frequency")# +
#theme_bw() +
#opts(panel.grid.major = theme_blank(),
#   panel.grid.minor = theme_blank(),
##   title="Conway's Word Cloud, Tweets (Obama -vs- Romney)",
#   plot.title = theme_text(family="Times", size=18))
# save plot in pdf
#ggsave("Obama_Romney_ModifyCloud.pdf", width=13, height=8, units="in")
    
</Code>
        <Arguments />
        <Referenced>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">q6oe</Variable>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">under40</Variable>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="28510.417400000002" width="1053" height="592" splitV="0.45000000000000001" change="51">
        <Ref>7f19695c-4b8a-4930-be8e-323b12432e66</Ref>
        <Name>or_df</Name>
        <Controls />
        <Code>clean.text &lt;- function(some_txt)
{ #https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/modified-cloud
   some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
   some_txt = gsub("@\\w+", "", some_txt)
   some_txt = gsub("[[:punct:]]", "", some_txt)
   some_txt = gsub("[[:digit:]]", "", some_txt)
   some_txt = gsub("http\\w+", "", some_txt)
   some_txt = gsub("[ \t]{2,}", "", some_txt)
   some_txt = gsub("^\\s+|\\s+$", "", some_txt)

   # define "tolower error handling" function
   try.tolower = function(x)
   {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
      y = tolower(x)
      return(y)
   }

   some_txt = sapply(some_txt, try.tolower)
   some_txt = some_txt[some_txt != ""]
   names(some_txt) = NULL
   return(some_txt)
}
# Cleaning the text
y = q6oe
x = under40#`Under 40`#d3
stop.words = c("Diet Coke","Coca-Cola","Coke Zero","Diet Pepsi","Pepsi Lite","Pepsi Max", "Coke","Pepsi")
                           
# cleaning the text                           
y = clean.text(y)
y = tolower(y)                           
dodgy.data = is.na(x) | y == ""
x = factor(x[!dodgy.data])
levs = levels(x)
lev1 = levs[1]                           
lev2 = levs[2]                           
y = y[!dodgy.data]

# stopwords
skipwords = c(stopwords("english"), stop.words, "Undefined")
skipwords = tolower(skipwords)
                           
# formatting the data
dat = c(paste(y[x == levs[1]], collapse=" "),paste(y[x == levs[2]], collapse=" "))                           
                           
# creating the corpuse and term-dociment matrix
or_corpus = Corpus(VectorSource(dat))                                                                                  
or_corpus = tm_map(or_corpus, removeWords, skipwords)                                                                                     
                                                                                    
# term-document matrix
tdm = TermDocumentMatrix(or_corpus)
tdm                                                                                     
                                                                                     
## Step 6: Create data frames from term-document matrix
# create data frame
or_df = as.data.frame(inspect(tdm))
names(or_df) = levs

# get rid of irrelevant words
expected = prop.table(apply(or_df,2,sum))
print(expected)
p.value &lt;- function(counts)  {
    p = tryCatch(chisq.test(counts, p = expected)$p.value,error=function(e) e, warning=function(w) w)
    if(is(p,"warning")) {
        #old &lt;- .Random.seed
        set.seed(2)
        p = chisq.test(counts, p = expected, simulate.p.value = TRUE, B = 100)$p.value
        #.Random.seed &lt;&lt;- old
    }
p        
}
                           
# computing relativities
        
        
p.values = apply(or_df,1,p.value)
or_df = or_df[p.values &lt; 0.25,]    
first.higher = (or_df[,1] / or_df[,2]) / (expected[1] / expected[1]) &gt; 0
or_df$first.higher = first.higher
        
df1 = or_df[first.higher,]
df2 = or_df[!first.higher,]
  #df2                                        
                           
#or_df = or_df[apply(or_df,2) &gt; 4, sum]

#Step 7: Create function to get the words spacing for the plot
# function
optimal.spacing &lt;- function(spaces)
{
   if(spaces &gt; 1) {
      spacing &lt;- 1 / spaces
      if(spaces%%2 &gt; 0) {
         lim = spacing * floor(spaces/2)
         return(seq(-lim, lim, spacing))
      }
      else {
         lim = spacing * (spaces-1)
         return(seq(-lim, lim, spacing*2))
      }
   }
   else {
      # add some jitter when 0
      return(jitter(0, amount=0.2))
   }
}
        
#Step 8: Apply function optimal.spacing
# spacing = sapply(table(or_df$first.higher), function(x) optimal.spacing(x))
# spacing1 = sapply(table(df1$first.higher), function(x) optimal.spacing(x))
# spacing2 = sapply(table(df2$first.higher), function(x) optimal.spacing(x))

# # add spacings
# optim1 = rep(0, length(spacing))
# for(n in names(spacing1)) {
#    optim1[df1$first == as.numeric(n)] &lt;- spacing1[[n]]
# }
# df1 = transform(df1, Spacing=optim1)
# df1
    
add.spacing = function(dat.frame) {
    spac = sapply(table(dat.frame$first.higher), function(x) optimal.spacing(x))
     print(spac)
     opt = rep(0, length(spac))
        print( names(spac))
#     for(n in names(spac)) {
#        opt[dat.frame$first == as.numeric(n)] &lt;- spac[[n]]
#     }
#     dat.frame = transform(dat.frame, Spacing = opt)
    dat.frame
}
    
    
df = add.spacing(or_df)    
df1 = add.spacing(df1)    
                  
#df2 = add.spacing(df2)    
#df2    
or_df
df1
                  # romney_optim = rep(0, nrow(romney_df))
# for(n in names(romney_spacing)) {
#    romney_optim[romney_df$freq.dif == as.numeric(n)] &lt;- romney_spacing[[n]]
# }
# romney_df = transform(romney_df, Spacing=romney_optim)

# both_df$Spacing = as.vector(both_spacing)
    
    
# spacing2                           

or_df                           
                           
                           
                           
                           </Code>
        <Arguments />
        <Referenced>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">q6oe</Variable>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">under40</Variable>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem width="1053" height="677" splitV="0.45000000000000001" change="55">
        <Ref>73d4c043-821c-4966-a647-629f483d311e</Ref>
        <Name>obj34</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem width="1053" height="648" splitV="0.45000000000000001" change="55">
        <Ref>6dc70c0a-645e-46dd-8f05-03514819b357</Ref>
        <Name>obj33</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="7634.7097000000003" width="1053" height="620" splitV="0.45000000000000001" change="51">
        <Ref>a13ef23c-cfc0-4880-a1cb-83c09650cfd2</Ref>
        <Name>obj42</Name>
        <Controls />
        <Code># load entropy library
library("entropy")
# a single variable
# observed counts for each bin
y = c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)
# shrinkage estimate of frequencies
freqs.shrink(y)
entropy.shrink(y)


# observed counts for two random variables
y1 = c(4, 2, 3, 1, 10, 4)
y2 = c(2, 3, 7, 1, 4, 3)
# shrinkage estimate of Kullback-Leibler divergence
KL.shrink(y1, y2)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="17618.804100000001" width="1053" height="564" splitV="0.45000000000000001" change="51">
        <Ref>7783b237-14bc-472a-8429-a870100adafd</Ref>
        <Name>obj43</Name>
        <Controls />
        <Code>library(lattice)
xyplot(incidence/size ~ period|herd, cbpp, type=c('g','p','l'),
layout=c(3,5), index.cond = function(x,y)max(y))</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="19158.230800000001" width="1053" height="564" splitV="0.45000000000000001" change="51">
        <Ref>c58fea8b-9456-4f5d-b1d1-fddfc16f9e25</Ref>
        <Name>obj44</Name>
        <Controls />
        <Code>lmer(Yield ~ 1 + (1 | Batch), Dyestuff)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Current charts</Name>
      <Ref>e54adf9c-5f4b-4a43-9203-566ac0f39482</Ref>
      <CategoricalPlot mainSeries="Percent" change="43">
        <Ref>3993ebf1-9841-41e0-b5ee-7c886ec6112b</Ref>
        <Name>Age</Name>
        <ConditionalFormats>
          <Use guid="e86182c9-f84c-4f11-90ef-e14a8fe7b986" />
        </ConditionalFormats>
        <Settings itemTypeSpecified="true" lastPlotType="Column plot" showYAxis="false">
          <CategoricalAxis labelsFont="/14" />
          <PreFlipOptions>
            <Table colSeries="" hideEmpty="false" ignoreNetandSum="true" rowSeries="" seriesTypes="Percent" />
          </PreFlipOptions>
          <Seriess _count="1" />
        </Settings>
        <PlotQuestion>9d2d99a2-9e25-421a-bc72-63a7499c5c7e</PlotQuestion>
      </CategoricalPlot>
    </Group>
    <Table change="2">
      <Ref>dc7d859f-ad78-4624-95eb-7b0386fca450</Ref>
      <Name>Q5</Name>
      <ShowMissing>[missing] Q5</ShowMissing>
      <ByItem>SUMMARY</ByItem>
      <TableSeries>
        <Name>Percent</Name>
      </TableSeries>
    </Table>
    <Table change="35">
      <Ref>d6052c5f-77ed-49bf-8cdb-2b28c260d6ff</Ref>
      <Name>Q5</Name>
      <ShowMissing>[missing] Q5</ShowMissing>
      <ByItem>SUMMARY</ByItem>
      <TableSeries>
        <Name>Percent</Name>
      </TableSeries>
    </Table>
    <Group>
      <Name>Back to Back Histograms</Name>
      <Ref>238f5ab2-77a1-4466-825e-96f206b0d491</Ref>
      <Table change="36">
        <Ref>ab4f8771-cd10-485e-83a2-0f42ca01b481</Ref>
        <Name>Table</Name>
      </Table>
      <RItem timetaken="7274.2817000000005" splitV="0.45000000000000001" change="36">
        <Ref>ddcb57c1-50c8-4548-8259-c2a4dc2a95d3</Ref>
        <Name>obj1</Name>
        <Controls />
        <Code>age &lt;- rnorm(1000,50,10)
sex &lt;- sample(c("female","male"),1000,TRUE)
out &lt;- histbackback(split(age, sex), probability=TRUE, xlim=c(-.06,.06),main = "Back to Back Histogram")
#! just adding color
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)

</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Boxplot</Name>
      <Ref>8ae28a1d-c31a-4bbb-b746-acc0ac625d67</Ref>
      <RItem timetaken="5143.8622000000005" splitV="0.45000000000000001" change="35">
        <Ref>4a49db83-679c-4107-9e70-a4e1aa1db8c0</Ref>
        <Name>obj38</Name>
        <Controls />
        <Code>data(breast)
boxplot(time_survival ~ event_survival, data = breast,names = c("",""), col="#0000ff22")</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Beeswarm</Name>
      <Ref>a691314b-70a1-4d43-9fc5-b11b52096862</Ref>
      <RItem timetaken="6127.0690000000004" splitV="0.45000000000000001" change="35">
        <Ref>cb00820a-74ec-44e6-959f-28ad492632e7</Ref>
        <Name>obj37</Name>
        <Controls />
        <Code>data(breast)
beeswarm(time_survival ~ event_survival, data = breast, method = "swarm", pch = 16, pwcol = as.numeric(ER), xlab = "", 
         ylab = "Follow-up time (months)", labels = c("Censored","Metastasis"))

</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="35">
        <Ref>f0abef7b-3fe2-4ee3-8640-53ec05b6cb53</Ref>
        <Name>obj39</Name>
        <Controls />
        <Code>data(breast)
beeswarm.out &lt;- beeswarm(time_survival ~ event_survival,
data = breast, method = "swarm",pwcol = ER, do.plot=FALSE)[, c(1, 2, 4, 6)]

colnames(beeswarm.out) &lt;- c("x", "y", "ER", "event_survival")
library(ggplot2)
library(plyr) # for round_any()
p &lt;- ggplot(beeswarm.out, aes(x, y))
p &lt;- p + xlab("")
p &lt;- p + scale_y_continuous(expression("Follow-up time (months)"))
p &lt;- p + geom_boxplot(aes(x, y, group = round_any(x, 1, round)), outlier.shape = NA)
p &lt;- p + geom_point(aes(colour = ER))
p &lt;- p + scale_x_continuous(breaks = c(1:2), labels = c("Censored", "Metastasis"), expand = c(0, 0.5))
print(p)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Correlograms</Name>
      <Ref>6bf0746d-6274-4c65-825c-f5c0f577b6ee</Ref>
      <RItem timetaken="4794.8362999999999" splitV="0.45000000000000001" change="33">
        <Ref>c7077325-745f-455a-9fea-94cfe291f50f</Ref>
        <Name>mtcars</Name>
        <Controls />
        <Code>data(mtcars)
mtcars</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="9138.7023000000008" splitV="0.45000000000000001" change="33">
        <Ref>c895d6f8-331a-4198-bc84-7e27c8dfe09e</Ref>
        <Name>obj32</Name>
        <Controls />
        <Code>library(corrgram)

corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order")
</Code>
        <Arguments />
        <Referenced>
          <RItem guid="c7077325-745f-455a-9fea-94cfe291f50f" qset="00000000-0000-0000-0000-000000000000">mtcars</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="8732.4629000000004" splitV="0.45000000000000001" change="34">
        <Ref>602e45d1-14b2-4677-a113-55cae8f40970</Ref>
        <Name>obj35</Name>
        <Controls />
        <Code>panel.ellipse.points &lt;- function(x, y, corr = NULL, col.regions, cor.method, ...) {
    if (!is.null(corr)) 
        return()
    plot.xy(xy.coords(x, y), type = "p", ...)
    box(col = "lightgray")
        
        if (!is.null(corr)) 
        return()
    dfn &lt;- 2
    dfd &lt;- length(x) - 1
    shape &lt;- var(cbind(x, y), na.rm = TRUE)
    keep &lt;- (!is.na(x) &amp; !is.na(y))
    center &lt;- c(mean(x[keep]), mean(y[keep]))
    radius &lt;- sqrt(dfn * qf(0.68, dfn, dfd))
    segments &lt;- 75
    angles &lt;- seq(0, 2 * pi, length = segments)
    unit.circle &lt;- cbind(cos(angles), sin(angles))
    ellipse.pts &lt;- t(center + radius * t(unit.circle %*% chol(shape)))
    ellx &lt;- ellipse.pts[, 1]
    elly &lt;- ellipse.pts[, 2]
    usr &lt;- par()$usr
    minx &lt;- usr[1]
    maxx &lt;- usr[2]
    miny &lt;- usr[3]
    maxy &lt;- usr[4]
    ellx &lt;- ifelse(ellx &lt; minx, minx, ellx)
    ellx &lt;- ifelse(ellx &gt; maxx, maxx, ellx)
    elly &lt;- ifelse(elly &lt; miny, miny, elly)
    elly &lt;- ifelse(elly &gt; maxy, maxy, elly)
    lines(ellx, elly, col = "gray30", ...)
    ok &lt;- is.finite(x) &amp; is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = 2/3, iter = 3), 
            col = "red", ...)

        
        
    }


corrgram(mtcars, order=TRUE, lower.panel=panel.pie, upper.panel=panel.ellipse.points, text.panel=panel.txt, 
         diag.panel=panel.minmax, main="Car Milage Data in PC2/PC1 Order")</Code>
        <Arguments />
        <Referenced>
          <RItem guid="c7077325-745f-455a-9fea-94cfe291f50f" qset="00000000-0000-0000-0000-000000000000">mtcars</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="5740.0484999999999" splitV="0.45000000000000001" change="34">
        <Ref>f0fd9d69-2bbd-4efc-8ffa-153e02216b23</Ref>
        <Name>obj36</Name>
        <Controls />
        <Code>data(decathlon)
R = cor(decathlon[, 1:10])
round(R, 3)
plotcorr(R, col = colorRampPalette(c("firebrick3", "white", "navy"))(10))</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Steam graphs</Name>
      <Ref>e0747732-1cdc-4a85-8981-6398845de77b</Ref>
      <RItem timetaken="5291.9673000000003" splitV="0.45000000000000001" change="32">
        <Ref>cf4792ab-6e41-42bc-8503-87d8647a29bf</Ref>
        <Name>obj29</Name>
        <Controls />
        <Code>#http://stackoverflow.com/questions/13084998/streamgraphs-in-r
plot.stacked &lt;- function(x,y, ylab="", xlab="", ncol=1, xlim=range(x, na.rm=T), ylim=c(0, 1.2*max(rowSums(y), na.rm=T)), border = NULL, col=rainbow(length(y[1,]))){

    plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, xaxs="i", yaxs="i", xlim=xlim, t="n")
    bottom=0*y[,1]
    for(i in 1:length(y[1,])){
        top=rowSums(as.matrix(y[,1:i]))
        polygon(c(x, rev(x)), c(top, rev(bottom)), border=border, col=col[i])
        bottom=top
    }
    abline(h=seq(0,200000, 10000), lty=3, col="grey")
    legend("topleft", rev(colnames(y)), ncol=ncol, inset = 0, fill=rev(col), bty="0", bg="white", cex=0.8, col=col)
    box()
}

set.seed(1)
m &lt;- 500
n &lt;- 15
x &lt;- seq(m)
y &lt;- matrix(0, nrow=m, ncol=n)
colnames(y) &lt;- seq(n)
for(i in seq(ncol(y))){
    mu &lt;- runif(1, min=0.25*m, max=0.75*m)
    SD &lt;- runif(1, min=5, max=30)
    TMP &lt;- rnorm(1000, mean=mu, sd=SD)
    HIST &lt;- hist(TMP, breaks=c(0,x), plot=FALSE)
    fit &lt;- smooth.spline(HIST$counts ~ HIST$mids)
    y[,i] &lt;- fit$y
}

    plot.stacked(x,y)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="5885.2834000000003" splitV="0.45000000000000001" change="32">
        <Ref>021a2518-0e30-4e7b-9b7e-9aa138a3672f</Ref>
        <Name>obj30</Name>
        <Controls />
        <Code>library(devtools)
source_url('https://gist.github.com/menugget/7864454/raw/f698da873766347d837865eecfa726cdf52a6c40/plot.stream.4.R')

set.seed(1)
m &lt;- 500
n &lt;- 50
x &lt;- seq(m)
y &lt;- matrix(0, nrow=m, ncol=n)
colnames(y) &lt;- seq(n)
for(i in seq(ncol(y))){
    mu &lt;- runif(1, min=0.25*m, max=0.75*m)
    SD &lt;- runif(1, min=5, max=30)
    TMP &lt;- rnorm(1000, mean=mu, sd=SD)
    HIST &lt;- hist(TMP, breaks=c(0,x), plot=FALSE)
    fit &lt;- smooth.spline(HIST$counts ~ HIST$mids)
    y[,i] &lt;- fit$y
}
y &lt;- replace(y, y&lt;0.01, 0)

#order by when 1st value occurs
ord &lt;- order(apply(y, 2, function(r) min(which(r&gt;0))))
y2 &lt;- y[, ord]
COLS &lt;- rainbow(ncol(y2))

par(mar=c(0,0,0,0), bty="n")
plot.stream(x,y2, axes=FALSE, xlim=c(100, 400), xaxs="i", center=TRUE, spar=0.2, frac.rand=0.1, col=COLS, border=1, lwd=0.1)
</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="32">
        <Ref>83b6425b-8c30-4e12-847b-23f68dc0713f</Ref>
        <Name>obj31</Name>
        <Controls />
        <Code>## GET SOME BASIC DATA
films&lt;-read.csv("box.csv")

## ALL OF THIS IS FAKING DATA
get_dist&lt;-function(n,g){

  dist&lt;-g-(abs(sort(g-abs(rnorm(n,g,g*runif(1))))))
  dist&lt;-c(0,dist-min(dist),0)
  dist&lt;-dist*g/sum(dist)
  return(dist)
}

get_dates&lt;-function(w){
  start&lt;-as.Date("01-01-00",format="%d-%m-%y")+ceiling(runif(1)*365)
  return(start+w)
}

films$WEEKS&lt;-ceiling(runif(1)*10)+6
f&lt;-ddply(films,.(RANK),function(df)expand.grid(RANK=df$RANK,WEEKGROSS=get_dist(df$WEEKS,df$GROSS)))
weekly&lt;-merge(films,f,by=("RANK"))


## GENERATE THE PLOT DATA
plot.data&lt;-ddply(weekly,.(RANK),summarise,NAME=NAME,WEEKDATE=get_dates(seq_along(WEEKS)*7),WEEKGROSS=ifelse(RANK %% 2 == 0,-WEEKGROSS,WEEKGROSS),GROSS=GROSS)

g&lt;-ggplot() + 

  geom_area(data=plot.data[plot.data$WEEKGROSS&gt;=0,],
            aes(x=WEEKDATE,
                ymin=0,
                y=WEEKGROSS,
                group=NAME,
                fill=cut(GROSS,c(seq(0,1000,100),Inf)))
            ,alpha=0.5,
            stat="smooth", fullrange=T,n=1000,
            colour="white",
            size=0.25,alpha=0.5) +

  geom_area(data=plot.data[plot.data$WEEKGROSS&lt;0,],
            aes(x=WEEKDATE,
                ymin=0,
                y=WEEKGROSS,
                group=NAME,
                fill=cut(GROSS,c(seq(0,1000,100),Inf)))
            ,alpha=0.5,
            stat="smooth", fullrange=T,n=1000,
            colour="white",
            size=0.25,alpha=0.5) +

  theme_bw() + 
  scale_fill_brewer(palette="RdPu",name="Gross\nEUR (M)") +
  ylab("") + xlab("")

b&lt;-ggplot_build(g)$data[[1]]
b.ymax&lt;-max(b$y)


## MAKE LABELS FOR GROSS &gt; 450M
labels&lt;-ddply(plot.data[plot.data$GROSS&gt;450,],.(RANK,NAME),summarise,x=median(WEEKDATE),y=ifelse(sum(WEEKGROSS)&gt;0,b.ymax,-b.ymax),GROSS=max(GROSS))
labels&lt;-ddply(labels,.(y&gt;0),transform,NAME=paste(NAME,GROSS),y=(y*1.1)+((seq_along(y)*20*(y/abs(y)))))

## PLOT
g + 
  geom_segment(data=labels,aes(x=x,xend=x,y=0,yend=y,label=NAME),size=0.5,linetype=2,color="purple",alpha=0.5) +
  geom_text(data=labels,aes(x,y,label=NAME),size=3)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>maps</Name>
      <Ref>8b194094-6f62-4e33-ba6e-1c769cc69b3d</Ref>
      <RItem timetaken="5049.0556999999999" splitV="0.45000000000000001" change="23">
        <Ref>88922616-f69d-4fb5-a19a-7faa348e58c2</Ref>
        <Name>obj16</Name>
        <Controls />
        <Code>crimes &lt;-data.frame(state = tolower(rownames(USArrests)), USArrests)
 
states_map &lt;-map_data("state")
 
ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) </Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="24">
        <Ref>f82e408f-cda5-4ad4-92a4-94e1b1732fd2</Ref>
        <Name>obj17</Name>
        <Controls />
        <Code>state_df &lt;- map_data("state")
choropleth &lt;- merge(county_df, unemp, by = c("state", "county"))
choropleth &lt;- choropleth[order(choropleth$order), ]
choropleth$rate_d &lt;- cut(choropleth$rate, breaks = c(seq(0, 10, by = 2), 35))
ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rate_d), colour = alpha("white", 1/2), size = 0.2) +
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  scale_fill_brewer(pal = "PuRd")</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="26">
        <Ref>13099076-c42d-4e56-b15b-d94d0be3292c</Ref>
        <Name>world2Hires</Name>
        <Controls />
        <Code>map('world2Hires')
world2Hires</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="5978.2403000000004" splitV="0.45000000000000001" change="27">
        <Ref>13c4301b-e0c4-431b-b525-df11b65badf5</Ref>
        <Name>obj19</Name>
        <Controls />
        <Code>data(countryExData)
sPDF &lt;- joinCountryData2Map( countryExData
,joinCode = "ISO3"
,nameJoinColumn = "ISO3V10")
#mapDevice() #create world map shaped window
mapCountryData(sPDF
,nameColumnToPlot='BIODIVERSITY')</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="28">
        <Ref>7a894d5b-87ef-4dfe-9d24-f5b837e7bd66</Ref>
        <Name>obj20</Name>
        <Controls />
        <Code>data(countryExData)
sPDF &lt;- joinCountryData2Map( countryExData
,joinCode = "ISO3"
,nameJoinColumn = "ISO3V10")

mapBubbles(sPDF,nameZSize="POP2005"
,nameZColour="REGION"
,colourPalette="rainbow"
,oceanCol="lightblue"
,landCol="wheat")</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="5741.8982999999998" splitV="0.45000000000000001" change="29">
        <Ref>151b9ca1-ab50-4978-aa35-90378e6a7438</Ref>
        <Name>obj21</Name>
        <Controls />
        <Code>dF &lt;- data.frame( country=c("USA", "denmark", "uk","Australia"), Offices=c("Q Office") )
sPDF &lt;-joinCountryData2Map( dF ,joinCode = "NAME" ,nameJoinColumn = "country")
mapCountryData(sPDF,nameColumnToPlot="Offices")</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="20678.73" splitV="0.45000000000000001" change="29">
        <Ref>571fefe2-1c28-4e21-9cd8-67110320da0b</Ref>
        <Name>obj22</Name>
        <Controls />
        <Code>library(WDI)
# http://data.worldbank.org/indicator/
# Improved sanitation facilities (% of popn)
indicator &lt;- "SH.STA.ACSN"
dFsanitation &lt;- WDI( indicator=indicator
, start=2005
, end=2005 )
library(rworldmap)
sPDFsanitation &lt;-
obj22 &lt;- joinCountryData2Map( dFsanitation
, nameJoinColumn='iso2c'
, joinCode='ISO2')
obj22</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="21003.765300000003" splitV="0.45000000000000001" change="29">
        <Ref>6e5ed31c-3014-4833-bc10-7155a828f251</Ref>
        <Name>sPDFsanitation</Name>
        <Controls />
        <Code>library(WDI)
# http://data.worldbank.org/indicator/
# Improved sanitation facilities (% of popn)
indicator &lt;- "SH.STA.ACSN"
dFsanitation &lt;- WDI( indicator=indicator
, start=2005
, end=2005 )
library(rworldmap)
sPDFsanitation &lt;- joinCountryData2Map( dFsanitation, , nameJoinColumn='iso2c', joinCode='ISO2')</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="8708.2517000000007" splitV="0.45000000000000001" change="29">
        <Ref>c8e57d30-afae-4a3b-b32c-91a07b843ae6</Ref>
        <Name>obj25</Name>
        <Controls />
        <Code>summary(sPDFsanitation)$data</Code>
        <Arguments />
        <Referenced>
          <RItem guid="6e5ed31c-3014-4833-bc10-7155a828f251" qset="00000000-0000-0000-0000-000000000000">sPDFsanitation</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="7472.6563000000006" splitV="0.45000000000000001" change="30">
        <Ref>ec42fb60-ed62-4597-a0de-7713305936dd</Ref>
        <Name>obj23</Name>
        <Controls />
        <Code>#library(RColorBrewer)
numCats &lt;- 5
indicator &lt;- "SH.STA.ACSN"
colourPalette &lt;- rev(brewer.pal(numCats, "PuBuGn"))
mapCountryData( sPDFsanitation, nameColumnToPlot= indicator, colourPalette=colourPalette, numCats=numCats )</Code>
        <Arguments />
        <Referenced>
          <RItem guid="6e5ed31c-3014-4833-bc10-7155a828f251" qset="00000000-0000-0000-0000-000000000000">sPDFsanitation</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="7573.0885000000007" splitV="0.45000000000000001" change="30">
        <Ref>186804cc-ce51-4690-ba62-ee7060aa9cb8</Ref>
        <Name>obj26</Name>
        <Controls />
        <Code>numCats &lt;- 5
indicator &lt;- "SH.STA.ACSN"
colourPalette &lt;- rev(brewer.pal(numCats, "PuBuGn"))

mapBubbles( sPDFsanitation, nameZSize = indicator, , nameZColour = 'blue', fill = FALSE )
</Code>
        <Arguments />
        <Referenced>
          <RItem guid="6e5ed31c-3014-4833-bc10-7155a828f251" qset="00000000-0000-0000-0000-000000000000">sPDFsanitation</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="6758.6273000000001" splitV="0.45000000000000001" change="30">
        <Ref>c471d29b-330e-4bfb-a6ac-8f96cdc44ae5</Ref>
        <Name>obj27</Name>
        <Controls />
        <Code>numCats &lt;- 5
indicator &lt;- "SH.STA.ACSN"
colourPalette &lt;- rev(brewer.pal(numCats, "PuBuGn"))

barplotCountryData( sPDFsanitation
, nameColumnToPlot=indicator
, nameCountryColumn = "NAME"
, numCats = numCats
, colourPalette = colourPalette
, na.last = NA
, decreasing = FALSE
, scaleSameInPanels = TRUE
, numPanels = 7
, cex = 1.1 )</Code>
        <Arguments />
        <Referenced>
          <RItem guid="6e5ed31c-3014-4833-bc10-7155a828f251" qset="00000000-0000-0000-0000-000000000000">sPDFsanitation</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="31">
        <Ref>059e7940-c4b3-4b79-9626-09cc175bbceb</Ref>
        <Name>obj28</Name>
        <Controls />
        <Code>dFsanitation2 &lt;- sPDFsanitation@data
indicator &lt;- "SH.STA.ACSN"

dFsanitation2$inverse &lt;- 100 - dFsanitation2[[indicator]]
treemap( dFsanitation2, index = c("GEO3major", "NAME"), vSize = "inverse")</Code>
        <Arguments />
        <Referenced>
          <RItem guid="6e5ed31c-3014-4833-bc10-7155a828f251" qset="00000000-0000-0000-0000-000000000000">sPDFsanitation</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <LocalFileItem change="25">
        <Ref>8b51671d-767a-47d8-b1dd-ea40f11de756</Ref>
        <Name>Local File</Name>
      </LocalFileItem>
    </Group>
    <Group>
      <Name>Funnel</Name>
      <Ref>572b0746-74f5-4864-8f08-36ea87693924</Ref>
      <Table change="18">
        <Ref>2dd2cffe-fad6-4c99-8157-3245b68f7146</Ref>
        <Name>Table</Name>
      </Table>
      <RItem timetaken="1822.6326000000001" splitV="0.45000000000000001" change="21">
        <Ref>063fb291-8c3b-424a-8e96-cf3719ed04fb</Ref>
        <Name>obj14</Name>
        <Controls />
        <Code>data(catheter)
a &lt;- meta.MH(n.trt, n.ctrl, col.trt, col.ctrl, data=catheter,
names=Name, subset=c(13,6,5,3,7,12,4,11,1,8,10,2))
funnelplot(a$logOR, a$selogOR)
funnelplot(a$logOR, a$selogOR,
plot.conf=TRUE, summ=a$logMH, mirror=TRUE)
funnelplot(a, plot.conf=TRUE)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="3691.3768" splitV="0.45000000000000001" change="18">
        <Ref>6c896e7d-d1a7-4838-a7fb-4a82910bd647</Ref>
        <Name>obj15</Name>
        <Controls />
        <Code>data(cochrane)
steroid &lt;- meta.MH(n.trt, n.ctrl, ev.trt, ev.ctrl,
names=name, data=cochrane)
tabletext&lt;-cbind(c("","Study",steroid$names,NA,"Summary"),
c("Deaths","(steroid)",cochrane$ev.trt,NA,NA),
c("Deaths","(placebo)", cochrane$ev.ctrl, NA,NA),
c("","OR",format(exp(steroid$logOR),digits=2),NA,format(exp(steroid$logMH),digits=2))
)
m&lt;- c(NA,NA,steroid$logOR,NA,steroid$logMH)
l&lt;- m-c(NA,NA,steroid$selogOR,NA,steroid$selogMH)*2
u&lt;- m+c(NA,NA,steroid$selogOR,NA,steroid$selogMH)*2
forestplot(tabletext,m,l,u,zero=0,is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
clip=c(log(0.1),log(2.5)), xlog=TRUE,
col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"))
forestplot(tabletext,m,l,u,zero=0,is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
clip=c(log(0.1),log(2.5)), xlog=TRUE, boxsize=0.75,
col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"))</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Calender Plots</Name>
      <Ref>b348bec2-bcf5-4345-8683-423d9bdca453</Ref>
      <RItem timetaken="2017.9960000000001" splitV="0.45000000000000001" change="17">
        <Ref>c961cb54-8b46-4c7d-8e74-a457853310ad</Ref>
        <Name>stock.data</Name>
        <Controls />
        <Code>stock &lt;- "MSFT"
start.date &lt;- "2012-01-01"
end.date &lt;- Sys.Date()
quote &lt;- paste("http://ichart.finance.yahoo.com/table.csv?s=",
                stock,
                "&amp;a=", substr(start.date,6,7),
                "&amp;b=", substr(start.date, 9, 10),
                "&amp;c=", substr(start.date, 1,4), 
                "&amp;d=", substr(end.date,6,7),
                "&amp;e=", substr(end.date, 9, 10),
                "&amp;f=", substr(end.date, 1,4),
                "&amp;g=d&amp;ignore=.csv", sep="")   
stock.data &lt;- read.csv(quote, as.is=TRUE)
stock.data$Date &lt;- as.Date(stock.data$Date)
stock.data</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="9410.9572000000007" splitV="0.45000000000000001" change="17">
        <Ref>2a3f0bd2-929f-4d07-88ba-84cb5066c808</Ref>
        <Name>obj4</Name>
        <Controls />
        <Code>source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
# Plot as calendar heatmap
calendarHeat(stock.data$Date, stock.data$Adj.Close,              varname="MSFT Adjusted Close")</Code>
        <Arguments />
        <Referenced>
          <RItem guid="c961cb54-8b46-4c7d-8e74-a457853310ad" qset="00000000-0000-0000-0000-000000000000">stock.data</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem timetaken="3759.1739000000002" splitV="0.45000000000000001" change="17">
        <Ref>82ca6963-219a-4080-a230-17ef55340fb7</Ref>
        <Name>obj13</Name>
        <Controls />
        <Code>calendarHeat = function (dates, values, ncolors = 99, color = "r2g", varname = "Values", 
    date.form = "%Y-%m-%d", ...) 
{ #source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
    require(lattice)
    require(grid)
    require(chron)
    if (class(dates) == "character" | class(dates) == "factor") {
        dates &lt;- strptime(dates, date.form)
    }
    caldat &lt;- data.frame(value = values, dates = dates)
    min.date &lt;- as.Date(paste(format(min(dates), "%Y"), "-1-1", 
        sep = ""))
    max.date &lt;- as.Date(paste(format(max(dates), "%Y"), "-12-31", 
        sep = ""))
    dates.f &lt;- data.frame(date.seq = seq(min.date, max.date, 
        by = "days"))
    caldat &lt;- data.frame(date.seq = seq(min.date, max.date, by = "days"), 
        value = NA)
    dates &lt;- as.Date(dates)
    caldat$value[match(dates, caldat$date.seq)] &lt;- values
    caldat$dotw &lt;- as.numeric(format(caldat$date.seq, "%w"))
    caldat$woty &lt;- as.numeric(format(caldat$date.seq, "%U")) + 
        1
    caldat$yr &lt;- as.factor(format(caldat$date.seq, "%Y"))
    caldat$month &lt;- as.numeric(format(caldat$date.seq, "%m"))
    yrs &lt;- as.character(unique(caldat$yr))
    d.loc &lt;- as.numeric()
    for (m in min(yrs):max(yrs)) {
        d.subset &lt;- which(caldat$yr == m)
        sub.seq &lt;- seq(1, length(d.subset))
        d.loc &lt;- c(d.loc, sub.seq)
    }
    caldat &lt;- cbind(caldat, seq = d.loc)
    r2b &lt;- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020")
    r2g &lt;- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
    w2b &lt;- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")
    assign("col.sty", get(color))
    calendar.pal &lt;- colorRampPalette((col.sty), space = "Lab")
    def.theme &lt;- lattice.getOption("default.theme")
    cal.theme &lt;- function() {
        theme &lt;- list(strip.background = list(col = "transparent"), 
            strip.border = list(col = "transparent"), axis.line = list(col = "transparent"), 
            par.strip.text = list(cex = 0.8))
    }
    lattice.options(default.theme = cal.theme)
    yrs &lt;- (unique(caldat$yr))
    nyr &lt;- length(yrs)
    print(cal.plot &lt;- levelplot(value ~ woty * dotw | yr, data = caldat, 
        as.table = TRUE, aspect = 0.12, layout = c(1, nyr%%7), 
        between = list(x = 0, y = c(1, 1)), strip = TRUE, main = paste("Calendar Heat Map of ", 
            varname, sep = ""), scales = list(x = list(at = c(seq(2.9, 
            52, by = 4.42)), labels = month.abb, alternating = c(1, 
            rep(0, (nyr - 1))), tck = 0, cex = 0.7), y = list(at = c(0, 
            1, 2, 3, 4, 5, 6), labels = c("Sunday", "Monday", 
            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
            alternating = 1, cex = 0.6, tck = 0)), xlim = c(0.4, 
            54.6), ylim = c(6.6, -0.6), cuts = ncolors - 1, col.regions = (calendar.pal(ncolors)), 
        xlab = "", ylab = "", colorkey = list(col = calendar.pal(ncolors), 
            width = 0.6, height = 0.5), subscripts = TRUE))
    panel.locs &lt;- trellis.currentLayout()
    for (row in 1:nrow(panel.locs)) {
        for (column in 1:ncol(panel.locs)) {
            if (panel.locs[row, column] &gt; 0) {
                trellis.focus("panel", row = row, column = column, 
                  highlight = FALSE)
                xyetc &lt;- trellis.panelArgs()
                subs &lt;- caldat[xyetc$subscripts, ]
                dates.fsubs &lt;- caldat[caldat$yr == unique(subs$yr), 
                  ]
                y.start &lt;- dates.fsubs$dotw[1]
                y.end &lt;- dates.fsubs$dotw[nrow(dates.fsubs)]
                dates.len &lt;- nrow(dates.fsubs)
                adj.start &lt;- dates.fsubs$woty[1]
                for (k in 0:6) {
                  if (k &lt; y.start) {
                    x.start &lt;- adj.start + 0.5
                  }
                  else {
                    x.start &lt;- adj.start - 0.5
                  }
                  if (k &gt; y.end) {
                    x.finis &lt;- dates.fsubs$woty[nrow(dates.fsubs)] - 
                      0.5
                  }
                  else {
                    x.finis &lt;- dates.fsubs$woty[nrow(dates.fsubs)] + 
                      0.5
                  }
                  grid.lines(x = c(x.start, x.finis), y = c(k - 
                    0.5, k - 0.5), default.units = "native", 
                    gp = gpar(col = "grey", lwd = 1))
                }
                if (adj.start &lt; 2) {
                  grid.lines(x = c(0.5, 0.5), y = c(6.5, y.start - 
                    0.5), default.units = "native", gp = gpar(col = "grey", 
                    lwd = 1))
                  grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), 
                    default.units = "native", gp = gpar(col = "grey", 
                      lwd = 1))
                  grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] - 
                    0.5, -0.5), default.units = "native", gp = gpar(col = "grey", 
                    lwd = 1))
                  if (dates.fsubs$dotw[dates.len] != 6) {
                    grid.lines(x = c(x.finis + 1, x.finis + 1), 
                      y = c(dates.fsubs$dotw[dates.len] - 0.5, 
                        -0.5), default.units = "native", gp = gpar(col = "grey", 
                        lwd = 1))
                  }
                  grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] - 
                    0.5, -0.5), default.units = "native", gp = gpar(col = "grey", 
                    lwd = 1))
                }
                for (n in 1:51) {
                  grid.lines(x = c(n + 1.5, n + 1.5), y = c(-0.5, 
                    6.5), default.units = "native", gp = gpar(col = "grey", 
                    lwd = 1))
                }
                x.start &lt;- adj.start - 0.5
                if (y.start &gt; 0) {
                  grid.lines(x = c(x.start, x.start + 1), y = c(y.start - 
                    0.5, y.start - 0.5), default.units = "native", 
                    gp = gpar(col = "black", lwd = 1.75))
                  grid.lines(x = c(x.start + 1, x.start + 1), 
                    y = c(y.start - 0.5, -0.5), default.units = "native", 
                    gp = gpar(col = "black", lwd = 1.75))
                  grid.lines(x = c(x.start, x.start), y = c(y.start - 
                    0.5, 6.5), default.units = "native", gp = gpar(col = "black", 
                    lwd = 1.75))
                  if (y.end &lt; 6) {
                    grid.lines(x = c(x.start + 1, x.finis + 1), 
                      y = c(-0.5, -0.5), default.units = "native", 
                      gp = gpar(col = "black", lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5, 
                      6.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, 
                      -0.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5, 
                      6.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                  }
                }
                else {
                  grid.lines(x = c(x.start, x.start), y = c(-0.5, 
                    6.5), default.units = "native", gp = gpar(col = "black", 
                    lwd = 1.75))
                }
                if (y.start == 0) {
                  if (y.end &lt; 6) {
                    grid.lines(x = c(x.start, x.finis + 1), y = c(-0.5, 
                      -0.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5, 
                      6.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, 
                      -0.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5, 
                      6.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                  }
                }
                for (j in 1:12) {
                  last.month &lt;- max(dates.fsubs$seq[dates.fsubs$month == 
                    j])
                  x.last.m &lt;- dates.fsubs$woty[last.month] + 
                    0.5
                  y.last.m &lt;- dates.fsubs$dotw[last.month] + 
                    0.5
                  grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, 
                    y.last.m), default.units = "native", gp = gpar(col = "black", 
                    lwd = 1.75))
                  if ((y.last.m) &lt; 6) {
                    grid.lines(x = c(x.last.m, x.last.m - 1), 
                      y = c(y.last.m, y.last.m), default.units = "native", 
                      gp = gpar(col = "black", lwd = 1.75))
                    grid.lines(x = c(x.last.m - 1, x.last.m - 
                      1), y = c(y.last.m, 6.5), default.units = "native", 
                      gp = gpar(col = "black", lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, 
                      6.5), default.units = "native", gp = gpar(col = "black", 
                      lwd = 1.75))
                  }
                }
            }
        }
        trellis.unfocus()
    }
    lattice.options(default.theme = def.theme)
}

# Plot as calendar heatmap
calendarHeat(date, q2a_3,              varname="Coke Zero when Out and About")</Code>
        <Arguments />
        <Referenced>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">date</Variable>
          <Variable guid="00000000-0000-0000-0000-000000000000" qset="75d4318f-9299-42d9-ac11-f017575c53d5">q2a_3</Variable>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="55">
        <Ref>669fb0ba-72a8-4f51-9ea3-a4e63f7b4108</Ref>
        <Name>obj12</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <RItem timetaken="1741.9716000000001" splitV="0.45000000000000001" change="2">
      <Ref>bf21b431-21cf-458d-97d4-3c993de299b0</Ref>
      <Name>brandGrid</Name>
      <Controls />
      <Code>brandGrid = round(matrix(apply(Q5,2, mean)*100,7))
dimnames(brandGrid) = list(Brand = c("Coke","Diet Coke","Coke Zero","Pepsi","Diet Pepsi","Pepsi Max","None of these"), Attribute = c("Feminine","Health-conscious","Innocent","Older","Open to new experiences","Rebellious","Sleepy","Traditional","Weight-conscious"))
brandGrid</Code>
      <Arguments />
      <Referenced>
        <Question guid="c2e02da9-0d39-4784-874c-044b5ad5031b" qset="75d4318f-9299-42d9-ac11-f017575c53d5">Q5</Question>
      </Referenced>
      <Version>0</Version>
    </RItem>
    <RItem splitV="0.45000000000000001" change="17">
      <Ref>2bf735e2-5527-4d56-815b-ceea0a522fd2</Ref>
      <Name>obj3</Name>
      <Controls />
      <Code>RankingPlot &lt;- function (x, x.locs = c(0.01, 0.99), y.locs = c(0, 1), steps = NULL, 
    sp.names = NULL, dt.tx = T, rsc = T, ln.st = NULL, rs.ln = c(3, 
        15), ln.cl = "RdYlGn", alpha = 0.7, leg = T, ...) 
{
    require(RColorBrewer)
    require(scales)
    if (length(x.locs) != 2 | length(y.locs) != 2) 
        stop("x and y dimensions must be two-element vectors")
    if (x.locs[1] &lt; 0 | x.locs[2] &gt; 1 | y.locs[1] &lt; 0 | y.locs[2] &gt; 
        1) 
        stop("x and y dimensions must in range of 0--1")
    dim.x &lt;- c(0, 1)
    dim.y &lt;- c(0, 1)
    wrn.val &lt;- F
    x[, 1] &lt;- as.character(x[, 1])
    tot.sp &lt;- ncol(x) - 1
    sp.col &lt;- 2:ncol(x)
    sp.orig &lt;- x[, sp.col]
    if (length(rs.ln) == 1) 
        rsc &lt;- F
    if (rsc) 
        x[, sp.col] &lt;- rescale(x[, sp.col], rs.ln)
    if (rsc == F &amp; leg) 
        leg &lt;- F
    first.ord &lt;- order(x[1, sp.col], decreasing = T)
    x[, sp.col] &lt;- x[, sp.col][, first.ord]
    names(x)[sp.col] &lt;- names(x)[sp.col][first.ord]
    names(x)[sp.col] &lt;- paste(1:tot.sp, names(x)[sp.col], sep = ". ")
    dt.dat.srt &lt;- vector("list", nrow(x))
    names(dt.dat.srt) &lt;- x[, 1]
    for (val in 1:nrow(x)) {
        tmp &lt;- t(x[val, sp.col])
        tmp &lt;- tmp[order(tmp, decreasing = T), , drop = F]
        dt.dat.srt[[val]] &lt;- tmp
    }
    plot(dim.x, dim.y, type = "n", axes = F, xlab = "", ylab = "", 
        ...)
    if (!is.null(steps)) 
        dt.dat.srt &lt;- dt.dat.srt[names(dt.dat.srt) %in% steps]
    if (leg) {
        y.locs[1] &lt;- 0.05 * diff(y.locs) + y.locs[1]
        leg.txt &lt;- format(round(seq(min(sp.orig), max(sp.orig), 
            length = 5), 2), nsmall = 2, digits = 2)
        leg.wds &lt;- seq(rs.ln[1], rs.ln[2], length = 5)
        legend("bottom", (y.locs[1] - y.olds)/2, col = alpha("black", 
            alpha), lwd = leg.wds, legend = leg.txt, bty = "n", 
            horiz = T)
    }
    x.vals &lt;- rep(seq(x.locs[1], x.locs[2], length = length(dt.dat.srt)), 
        each = tot.sp)
    x.vals &lt;- split(x.vals, x.vals)
    if (dt.tx) 
        y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp + 
            1))[-1]
    else y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp))
    if (length(ln.cl) == 1) 
        if (ln.cl %in% row.names(brewer.pal.info)) {
            pal.num &lt;- brewer.pal.info[row.names(brewer.pal.info) == 
                ln.cl, 1]
            ln.cl &lt;- brewer.pal(pal.num, ln.cl)
        }
    line.cols &lt;- alpha(colorRampPalette(ln.cl)(tot.sp), alpha)
    if (is.null(ln.st)) {
        str.max &lt;- max(strwidth(row.names(dt.dat.srt[[1]])))
        if (diff(x.locs) - length(dt.dat.srt) * str.max &lt; 0) {
            warning("not enough space for lines between columns")
            wrn.val &lt;- T
        }
        else ln.st &lt;- 0.2 * str.max + str.max/2
    }
    for (val in 1:(length(dt.dat.srt) - 1)) {
        plt.tmp &lt;- dt.dat.srt[c(val, val + 1)]
        x.tmp &lt;- x.vals[c(val, val + 1)]
        text(x.tmp[[1]], y.vals, row.names(plt.tmp[[1]]))
        if (val == length(dt.dat.srt) - 1) {
            text(x.tmp[[2]], y.vals, row.names(plt.tmp[[2]]))
            if (dt.tx) {
                dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[2]))
                text(unique(x.tmp[[2]]), y.locs[2], dt.txt)
            }
        }
        if (dt.tx) {
            dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[1]))
            text(unique(x.tmp[[1]]), y.locs[2], dt.txt)
        }
        srt.ln.y &lt;- match(row.names(plt.tmp[[1]]), row.names(plt.tmp[[2]]))
        if (rsc) 
            lwd.val &lt;- plt.tmp[[1]][, 1]
        else lwd.val &lt;- rep(rs.ln[1], tot.sp)
        if (is.null(sp.names)) 
            sel.sp &lt;- rep(T, tot.sp)
        else {
            sel.names &lt;- unlist(lapply(strsplit(row.names(plt.tmp[[1]]), 
                " "), function(x) x[2]))
            sel.sp &lt;- (sel.names %in% sp.names)
        }
        if (!wrn.val) 
            segments(x.tmp[[1]][sel.sp] + ln.st, y.vals[sel.sp], 
                x.tmp[[2]][sel.sp] - ln.st, y.vals[srt.ln.y][sel.sp], 
                col = line.cols[sel.sp], lwd = lwd.val[sel.sp])
        srt.cl.y &lt;- match(row.names(plt.tmp[[2]]), row.names(plt.tmp[[1]]))
        line.cols &lt;- line.cols[srt.cl.y]
    }
}
                                       
RankingPlot(brandGrid)</Code>
      <Arguments />
      <Referenced>
        <RItem guid="bf21b431-21cf-458d-97d4-3c993de299b0" qset="00000000-0000-0000-0000-000000000000">brandGrid</RItem>
        <RItem guid="376067cf-b8de-43d0-938d-6efb9552a538" qset="00000000-0000-0000-0000-000000000000">plot.qual</RItem>
      </Referenced>
      <Version>0</Version>
    </RItem>
    <RItem timetaken="1537.3173000000002" splitV="0.45000000000000001" change="17">
      <Ref>376067cf-b8de-43d0-938d-6efb9552a538</Ref>
      <Name>sp.dat</Name>
      <Controls />
      <Code>#create random data
set.seed(5)
 
#time steps
step&lt;-as.character(seq(2001,2012))
 
#species names
sp&lt;-paste0('Brand Name',seq(1,25))
 
#random data for species frequency occurrence
sp.dat&lt;-runif(length(step)*length(sp),0,15)
 
#create data frame for use with plot
sp.dat&lt;-matrix(sp.dat,nrow=length(step),ncol=length(sp))
sp.dat&lt;-data.frame(step,sp.dat)
names(sp.dat)&lt;-c('step',sp)


#reassign values of four variables
 
#pick random species names
vars&lt;-sp[sp %in% sample(sp,4)]
 
#function for getting value at each time step
time.fun&lt;-function(strt.val,steps,mean.val,lim.val){
    step&lt;-1
    x.out&lt;-strt.val
    while(step&lt;steps){
        x.new&lt;-x.out[step] + rnorm(1,mean=mean.val)
        x.out&lt;-c(x.out,x.new)
        step&lt;-step+1
        }
    if(lim.val&lt;=0) return(pmax(lim.val,x.out))
    else return(pmin(lim.val,x.out))
    }
 
#use function to reassign variable values
sp.dat[,vars[1]]&lt;-time.fun(14.5,6,-3,0) #start at 14.5, decrease rapidly
sp.dat[,vars[2]]&lt;-time.fun(13.5,6,-1,0) #start at 13.5, decrease less rapidly
sp.dat[,vars[3]]&lt;-time.fun(0.5,6,3,15) #start at 0.5, increase rapidly
sp.dat[,vars[4]]&lt;-time.fun(1.5,6,1,15) #start at 1.5, increase less rapidly
dimnames(sp.dat)[[1]] &lt;- step
sp.dat#  = sp.dat[,-1]

        </Code>
      <Arguments />
      <Referenced />
      <Version>0</Version>
    </RItem>
    <RItem timetaken="2060.8396000000002" splitV="0.45000000000000001" change="17">
      <Ref>e0d884b2-76d0-40bc-b625-4161d8155c97</Ref>
      <Name>obj2</Name>
      <Controls />
      <Code>source_gist('5281518')
par(mar=numeric(4),family='serif')
plot.qual(sp.dat,rs.ln=c(3,15))
</Code>
      <Arguments />
      <Referenced>
        <RItem guid="376067cf-b8de-43d0-938d-6efb9552a538" qset="00000000-0000-0000-0000-000000000000">sp.dat</RItem>
      </Referenced>
      <Version>0</Version>
    </RItem>
    <Group>
      <Name>Ranking Plot Over Time</Name>
      <Ref>6744796b-68e0-4a54-a037-b10a47dfc2cd</Ref>
      <RItem timetaken="2160.6727000000001" splitV="0.45000000000000001" change="17">
        <Ref>0186770d-3ce6-43e7-9268-bcb2659160d7</Ref>
        <Name>obj6</Name>
        <Controls />
        <Code>

RankingPlot &lt;- function (x, x.locs = c(0.01, 0.99), y.locs = c(0, 1), steps = NULL, 
    sp.names = NULL, dt.tx = T, rsc = T, ln.st = NULL, line.width = c(3, 
        15), ln.cl = "RdYlGn", alpha = 0.7, leg = T, text.cex = 1,  ...) 
{   # Adapted from http://www.r-bloggers.com/a-nifty-line-plot-to-visualize-multivariate-time-series/
    require(RColorBrewer)
    require(scales)
    if (length(x.locs) != 2 | length(y.locs) != 2) 
        stop("x and y dimensions must be two-element vectors")
    if (x.locs[1] &lt; 0 | x.locs[2] &gt; 1 | y.locs[1] &lt; 0 | y.locs[2] &gt; 
        1) 
        stop("x and y dimensions must in range of 0--1")
    dim.x &lt;- c(0, 1)
    dim.y &lt;- c(0, 1)
    wrn.val &lt;- F
    #x[, 1] &lt;- as.character(x[, 1])
    tot.sp &lt;- ncol(x) - 1 ##need to change
    sp.col &lt;- 2:ncol(x) ##need to change
    sp.orig &lt;- x[, sp.col]  ##need to change
    if (length(line.width) == 1) 
        rsc &lt;- F
    if (rsc) 
        x[, sp.col] &lt;- rescale(x[, sp.col], line.width)
    if (rsc == F &amp; leg) 
        leg &lt;- F
    first.ord &lt;- sp.col#order(x[1, sp.col], decreasing = T)
    
    #x[, sp.col] &lt;- x[, sp.col][, first.ord]
    #names(x)[sp.col] &lt;- names(x)[sp.col][first.ord]
    #names(x)[sp.col] &lt;- paste(1:tot.sp, names(x)[sp.col], sep = ". ")
    names(x) &lt;- names(x)[first.ord]
    names(x) &lt;- paste(1:tot.sp, names(x), sep = ". ")
    dt.dat.srt &lt;- vector("list", nrow(x))
    names(dt.dat.srt) &lt;- x[, 1]
    for (val in 1:nrow(x)) {
        tmp &lt;- t(x[val, sp.col])
        tmp &lt;- tmp[order(tmp, decreasing = T), , drop = F]
        dt.dat.srt[[val]] &lt;- tmp
    }
    plot(dim.x, dim.y, type = "n", axes = F, xlab = "", ylab = "", 
        ...)
    if (!is.null(steps)) 
        dt.dat.srt &lt;- dt.dat.srt[names(dt.dat.srt) %in% steps]
    if (leg) {
        y.locs[1] &lt;- 0.05 * diff(y.locs) + y.locs[1]
        leg.txt &lt;- format(round(seq(min(sp.orig), max(sp.orig), length = 5), 2), nsmall = 2, digits = 2)
        leg.wds &lt;- seq(line.width[1], line.width[2], length = 5)
        legend("bottom", (y.locs[1] - y.olds)/2, col = alpha("black", 
            alpha), lwd = leg.wds, legend = leg.txt, bty = "n", 
            horiz = T)
    }
    x.vals &lt;- rep(seq(x.locs[1], x.locs[2], length = length(dt.dat.srt)), 
        each = tot.sp)
    x.vals &lt;- split(x.vals, x.vals)
    if (dt.tx) 
        y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp + 
            1))[-1]
    else y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp))
    if (length(ln.cl) == 1) 
        if (ln.cl %in% row.names(brewer.pal.info)) {
            pal.num &lt;- brewer.pal.info[row.names(brewer.pal.info) == 
                ln.cl, 1]
            ln.cl &lt;- brewer.pal(pal.num, ln.cl)
        }
    line.cols &lt;- alpha(colorRampPalette(ln.cl)(tot.sp), alpha)
    if (is.null(ln.st)) {
        str.max &lt;- max(strwidth(row.names(dt.dat.srt[[1]])), cex = text.cex) / 4
        if (diff(x.locs) - length(dt.dat.srt) * str.max &lt; 0) {
            warning("not enough space for lines between columns")
            wrn.val &lt;- T
        }
        else ln.st &lt;- 0.2 * str.max + str.max/2
    }
        
     for (val in 1:(length(dt.dat.srt) - 1)) {
         # drawing text
         color = adjustcolor('black', .8)
         plt.tmp &lt;- dt.dat.srt[c(val, val + 1)]
         x.tmp &lt;- x.vals[c(val, val + 1)]
         x.centers = x.tmp[[1]] 
         text.width = str.max
         text.height = (y.vals[1] - y.vals[2])/4 
         rect(x.centers - text.width/ 2,y.vals - text.height,x.centers + text.width/ 2,y.vals + text.height, col=color, str = NULL)
         text(x.centers, y.vals, row.names(plt.tmp[[1]]), cex = text.cex, col = 'white')

         #shadowtext(x.tmp[[1]], y.vals, row.names(plt.tmp[[1]]))
#         if (val == length(dt.dat.srt) - 1) {
#             text(x.tmp[[2]], y.vals, row.names(plt.tmp[[2]]))
#             if (dt.tx) {
#                 dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[2]))
#                 text(unique(x.tmp[[2]]), y.locs[2], dt.txt)
#             }
#         }
#         if (dt.tx) {
#             dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[1]))
#             text(unique(x.tmp[[1]]), y.locs[2], dt.txt)
#         }
#         srt.ln.y &lt;- match(row.names(plt.tmp[[1]]), row.names(plt.tmp[[2]]))
#         if (rsc) 
#             lwd.val &lt;- plt.tmp[[1]][, 1]
#         else lwd.val &lt;- rep(line.width[1], tot.sp)
#         if (is.null(sp.names)) 
#             sel.sp &lt;- rep(T, tot.sp)
#         else {
#             sel.names &lt;- unlist(lapply(strsplit(row.names(plt.tmp[[1]]), 
#                 " "), function(x) x[2]))
#             sel.sp &lt;- (sel.names %in% sp.names)
#         }
#         if (!wrn.val) 
#             segments(x.tmp[[1]][sel.sp] + ln.st, y.vals[sel.sp], 
#                 x.tmp[[2]][sel.sp] - ln.st, y.vals[srt.ln.y][sel.sp], 
#                 col = line.cols[sel.sp], lwd = lwd.val[sel.sp])
#         srt.cl.y &lt;- match(row.names(plt.tmp[[2]]), row.names(plt.tmp[[1]]))
#         line.cols &lt;- line.cols[srt.cl.y]
    }
}
                                       
RankingPlot(sp.dat,line.width=c(3,15), text.cex = .3)</Code>
        <Arguments />
        <Referenced>
          <RItem guid="376067cf-b8de-43d0-938d-6efb9552a538" qset="00000000-0000-0000-0000-000000000000">sp.dat</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="8">
        <Ref>dee406e0-588b-423c-8f26-3e9a46686d9c</Ref>
        <Name>obj10</Name>
        <Controls />
        <Code>#create random data
set.seed(5)
 
#time steps
step&lt;-as.character(seq(2007,2012))
 
#species names
sp&lt;-paste0('sp',seq(1,25))
 
#random data for species frequency occurrence
sp.dat&lt;-runif(length(step)*length(sp),0,15)
 
#create data frame for use with plot
sp.dat&lt;-matrix(sp.dat,nrow=length(step),ncol=length(sp))
sp.dat&lt;-data.frame(step,sp.dat)
names(sp.dat)&lt;-c('step',sp)


#reassign values of four variables
 
#pick random species names
vars&lt;-sp[sp %in% sample(sp,4)]
 
#function for getting value at each time step
time.fun&lt;-function(strt.val,steps,mean.val,lim.val){
    step&lt;-1
    x.out&lt;-strt.val
    while(step&lt;steps){
        x.new&lt;-x.out[step] + rnorm(1,mean=mean.val)
        x.out&lt;-c(x.out,x.new)
        step&lt;-step+1
        }
    if(lim.val&lt;=0) return(pmax(lim.val,x.out))
    else return(pmin(lim.val,x.out))
    }
 
#use function to reassign variable values
sp.dat[,vars[1]]&lt;-time.fun(14.5,6,-3,0) #start at 14.5, decrease rapidly
sp.dat[,vars[2]]&lt;-time.fun(13.5,6,-1,0) #start at 13.5, decrease less rapidly
sp.dat[,vars[3]]&lt;-time.fun(0.5,6,3,15) #start at 0.5, increase rapidly
sp.dat[,vars[4]]&lt;-time.fun(1.5,6,1,15) #start at 1.5, increase less rapidly
sp.dat

        
        x = sp.dat

rs.ln=c(3,15)
require(RColorBrewer)
require(scales)

x.locs = c(0.01, 0.99)
y.locs = c(0, 1)
steps = NULL
sp.names = NULL
dt.tx = T
rsc = T
ln.st = NULL
ln.cl = "RdYlGn"
alpha = 0.7
leg = T
   # Adapted from http://www.r-bloggers.com/a-nifty-line-plot-to-visualize-multivariate-time-series/
    if (length(x.locs) != 2 | length(y.locs) != 2) 
        stop("x and y dimensions must be two-element vectors")
    if (x.locs[1] &lt; 0 | x.locs[2] &gt; 1 | y.locs[1] &lt; 0 | y.locs[2] &gt; 
        1) 
        stop("x and y dimensions must in range of 0--1")
    dim.x &lt;- c(0, 1)
    dim.y &lt;- c(0, 1)
    wrn.val &lt;- F
    x[, 1] &lt;- as.character(x[, 1])
    tot.sp &lt;- ncol(x) - 1
    sp.col &lt;- 2:ncol(x)
    sp.orig &lt;- x[, sp.col]
    if (length(rs.ln) == 1) 
        rsc &lt;- F
    if (rsc) 
        x[, sp.col] &lt;- rescale(x[, sp.col], rs.ln)
    if (rsc == F &amp; leg) 
        leg &lt;- F
    first.ord &lt;- order(x[1, sp.col], decreasing = T)
    x[, sp.col] &lt;- x[, sp.col][, first.ord]
    names(x)[sp.col] &lt;- names(x)[sp.col][first.ord]
    names(x)[sp.col] &lt;- paste(1:tot.sp, names(x)[sp.col], sep = ". ")
    dt.dat.srt &lt;- vector("list", nrow(x))
    names(dt.dat.srt) &lt;- x[, 1]
    for (val in 1:nrow(x)) {
        tmp &lt;- t(x[val, sp.col])
        tmp &lt;- tmp[order(tmp, decreasing = T), , drop = F]
        dt.dat.srt[[val]] &lt;- tmp
    }
    zplot = plot(dim.x, dim.y, type = "n", axes = F, xlab = "", ylab = "")
    if (!is.null(steps)) 
        dt.dat.srt &lt;- dt.dat.srt[names(dt.dat.srt) %in% steps]
    if (leg) {
        y.locs[1] &lt;- 0.05 * diff(y.locs) + y.locs[1]
        leg.txt &lt;- format(round(seq(min(sp.orig), max(sp.orig), 
            length = 5), 2), nsmall = 2, digits = 2)
        leg.wds &lt;- seq(rs.ln[1], rs.ln[2], length = 5)
        legend("bottom", (y.locs[1] - y.olds)/2, col = alpha("black", 
            alpha), lwd = leg.wds, legend = leg.txt, bty = "n", 
            horiz = T)
    }
zplot
        
    x.vals &lt;- rep(seq(x.locs[1], x.locs[2], length = length(dt.dat.srt)), 
        each = tot.sp)
    x.vals &lt;- split(x.vals, x.vals)
    if (dt.tx) 
        y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp + 
            1))[-1]
    else y.vals &lt;- rev(seq(y.locs[1], y.locs[2], length = tot.sp))
    if (length(ln.cl) == 1) 
        if (ln.cl %in% row.names(brewer.pal.info)) {
            pal.num &lt;- brewer.pal.info[row.names(brewer.pal.info) == 
                ln.cl, 1]
            ln.cl &lt;- brewer.pal(pal.num, ln.cl)
        }
    line.cols &lt;- alpha(colorRampPalette(ln.cl)(tot.sp), alpha)
    if (is.null(ln.st)) {
        str.max &lt;- max(strwidth(row.names(dt.dat.srt[[1]])))
        if (diff(x.locs) - length(dt.dat.srt) * str.max &lt; 0) {
            warning("not enough space for lines between columns")
            wrn.val &lt;- T
        }
        else ln.st &lt;- 0.2 * str.max + str.max/2
    }
    for (val in 1:(length(dt.dat.srt) - 1)) {
        plt.tmp &lt;- dt.dat.srt[c(val, val + 1)]
        x.tmp &lt;- x.vals[c(val, val + 1)]
        text(x.tmp[[1]], y.vals, row.names(plt.tmp[[1]]))
        if (val == length(dt.dat.srt) - 1) {
            text(x.tmp[[2]], y.vals, row.names(plt.tmp[[2]]))
            if (dt.tx) {
                dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[2]))
                text(unique(x.tmp[[2]]), y.locs[2], dt.txt)
            }
        }
        if (dt.tx) {
            dt.txt &lt;- substitute(italic(x), list(x = names(plt.tmp)[1]))
            text(unique(x.tmp[[1]]), y.locs[2], dt.txt)
        }
        srt.ln.y &lt;- match(row.names(plt.tmp[[1]]), row.names(plt.tmp[[2]]))
        if (rsc) 
            lwd.val &lt;- plt.tmp[[1]][, 1]
        else lwd.val &lt;- rep(rs.ln[1], tot.sp)
        if (is.null(sp.names)) 
            sel.sp &lt;- rep(T, tot.sp)
        else {
            sel.names &lt;- unlist(lapply(strsplit(row.names(plt.tmp[[1]]), 
                " "), function(x) x[2]))
            sel.sp &lt;- (sel.names %in% sp.names)
        }
        if (!wrn.val) 
            segments(x.tmp[[1]][sel.sp] + ln.st, y.vals[sel.sp], 
                x.tmp[[2]][sel.sp] - ln.st, y.vals[srt.ln.y][sel.sp], 
                col = line.cols[sel.sp], lwd = lwd.val[sel.sp])
        srt.cl.y &lt;- match(row.names(plt.tmp[[2]]), row.names(plt.tmp[[1]]))
        line.cols &lt;- line.cols[srt.cl.y]
    }
        </Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem timetaken="1493.6268" splitV="0.45000000000000001" change="10">
        <Ref>9fc5e48a-38da-49c6-8c8e-429829dc2a26</Ref>
        <Name>obj11</Name>
        <Controls />
        <Code>require(grid)
tg1 = textGrob("Sample",name = "tg1")
rg1 = rectGrob(width = 1.1 * grobWidth(tg1), height = 1.1 * grobHeight(tg1), gp = gpar(col = "grey60", lwd = 3))
rg2 = rectGrob(width = 1.1 * grobWidth(tg1), height = 1.3 * grobHeight(tg1), gp = gpar(col = "white", lwd = 3))
               
               
grid.draw(tg1)
grid.draw(rg1)
grid.draw(rg2)

</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
    <Group>
      <Name>Facets</Name>
      <Ref>658b2ffa-f644-4fbc-90b2-e93bfea53e61</Ref>
      <RItem timetaken="19062.216800000002" splitV="0.45000000000000001" change="6">
        <Ref>4c54a63f-e573-49be-80b6-f855a2bc426e</Ref>
        <Name>obj7</Name>
        <Controls />
        <Code>library(ggplot2)
data(diamonds)
ggScatter = ggplot(diamonds, aes(x = carat, y = price)) + geom_point()
ggScatter + facet_grid(cut~color)</Code>
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="6">
        <Ref>23a0aade-f881-489b-928a-37da59f91949</Ref>
        <Name>z</Name>
        <Controls />
        <Code>z = obj7 + facet_wrap(~color)
z</Code>
        <Arguments />
        <Referenced>
          <RItem guid="4c54a63f-e573-49be-80b6-f855a2bc426e" qset="00000000-0000-0000-0000-000000000000">obj7</RItem>
        </Referenced>
        <Version>0</Version>
      </RItem>
      <RItem splitV="0.45000000000000001" change="55">
        <Ref>922e16a3-d3d7-466d-9511-e57d816272b2</Ref>
        <Name>obj9</Name>
        <Controls />
        <Code />
        <Arguments />
        <Referenced />
        <Version>0</Version>
      </RItem>
    </Group>
  </Analyses>
  <Styles>
    <TableStyle ref="b9d1bf41-02e8-48d9-a06c-03e224050a86" name="Sobre" fileVer="0.3">
      <HeaderFont family="Calibri" size="12" style="1" />
      <FooterFont family="Calibri" size="9" style="0" />
      <ColFont family="Calibri" size="9.75" style="0" />
      <RowFont family="Calibri" size="9.75" style="0" />
      <TableFont family="Calibri" size="9.75" style="0" />
      <SeriesFont family="Calibri" size="9" style="0" />
      <HeaderColour>-16777216</HeaderColour>
      <FooterColour>-16777216</FooterColour>
      <ColColour>-16777216</ColColour>
      <RowColour>-16777216</RowColour>
      <TableColour>-16777216</TableColour>
      <SeriesColour>-16777216</SeriesColour>
      <SigColourPos>-16776961</SigColourPos>
      <SigColourNeg>-65536</SigColourNeg>
      <BorderColour>-16777216</BorderColour>
      <ShrinkColumns />
      <InlineSeries />
      <InlineQNames />
      <BoldNets />
      <MinimalBorders />
      <ExportFooterSep />
      <SigArrows />
      <SigFontColour />
      <NColsToFit>15</NColsToFit>
      <ShowMissingAs>NaN</ShowMissingAs>
    </TableStyle>
    <TableStyle ref="47900440-6877-4e53-803f-87015d0abb79" name="Colorful" fileVer="0.3">
      <HeaderFont family="Arial" size="12" style="1" />
      <FooterFont family="Arial" size="9" style="0" />
      <ColFont family="Arial" size="8.25" style="0" />
      <RowFont family="Arial" size="8.25" style="0" />
      <TableFont family="Arial" size="8.25" style="0" />
      <SeriesFont family="Arial" size="8.25" style="0" />
      <HeaderColour>-16777216</HeaderColour>
      <FooterColour>-16777216</FooterColour>
      <ColColour>-16777216</ColColour>
      <RowColour>-16777216</RowColour>
      <TableColour>-16777216</TableColour>
      <SeriesColour>-16777216</SeriesColour>
      <SigColourPos>-16776961</SigColourPos>
      <SigColourNeg>-65536</SigColourNeg>
      <BorderColour>-16777216</BorderColour>
      <ShowGraph />
      <ShrinkColumns />
      <ColourHeaders />
      <PlaceFooterAtMargin />
      <ExportFooterSep />
      <SigArrows />
      <SigFontColour />
      <NColsToFit>15</NColsToFit>
      <ShowMissingAs>NaN</ShowMissingAs>
    </TableStyle>
  </Styles>
  <History>
    <Created change="1" date="Sunday, 22 February 2015" time="1:22:02 PM" user="Tim" />
    <Modified change="2" date="Sunday, 22 February 2015" time="1:32:06 PM" user="Tim" />
    <Modified change="3" date="Sunday, 22 February 2015" time="1:57:07 PM" user="Tim" />
    <Modified change="4" date="Sunday, 22 February 2015" time="2:05:46 PM" user="Tim" />
    <Modified change="5" date="Sunday, 22 February 2015" time="2:25:56 PM" user="Tim" />
    <Modified change="6" date="Sunday, 22 February 2015" time="2:36:43 PM" user="Tim" />
    <Modified change="7" date="Sunday, 22 February 2015" time="2:46:43 PM" user="Tim" />
    <Modified change="8" date="Sunday, 22 February 2015" time="2:56:43 PM" user="Tim" />
    <Modified change="9" date="Sunday, 22 February 2015" time="3:06:44 PM" user="Tim" />
    <Modified change="10" date="Sunday, 22 February 2015" time="3:27:45 PM" user="Tim" />
    <Modified change="11" date="Sunday, 22 February 2015" time="3:37:45 PM" user="Tim" />
    <Modified change="12" date="Sunday, 22 February 2015" time="3:48:25 PM" user="Tim" />
    <Modified change="13" date="Sunday, 22 February 2015" time="5:39:27 PM" user="Tim" />
    <Modified change="14" date="Sunday, 22 February 2015" time="6:06:52 PM" user="Tim" />
    <Modified change="15" date="Sunday, 22 February 2015" time="6:07:21 PM" user="Tim" />
    <Modified change="16" date="Sunday, 22 February 2015" time="6:18:51 PM" user="Tim" />
    <Modified change="17" date="Sunday, 22 February 2015" time="6:38:54 PM" user="Tim" />
    <Modified change="18" date="Sunday, 22 February 2015" time="7:11:52 PM" user="Tim" />
    <Modified change="19" date="Monday, 23 February 2015" time="6:32:20 AM" user="Tim" />
    <Modified change="20" date="Monday, 23 February 2015" time="10:23:37 AM" user="Tim" />
    <Modified change="21" date="Monday, 23 February 2015" time="11:35:07 AM" user="Tim" />
    <Modified change="22" date="Monday, 23 February 2015" time="12:06:05 PM" user="Tim" />
    <Modified change="23" date="Monday, 23 February 2015" time="12:16:05 PM" user="Tim" />
    <Modified change="24" date="Monday, 23 February 2015" time="12:21:57 PM" user="Tim" />
    <Modified change="25" date="Monday, 23 February 2015" time="12:24:14 PM" user="Tim" />
    <Modified change="26" date="Monday, 23 February 2015" time="12:32:37 PM" user="Tim" />
    <Modified change="27" date="Monday, 23 February 2015" time="12:42:45 PM" user="Tim" />
    <Modified change="28" date="Monday, 23 February 2015" time="12:52:48 PM" user="Tim" />
    <Modified change="29" date="Monday, 23 February 2015" time="1:02:55 PM" user="Tim" />
    <Modified change="30" date="Monday, 23 February 2015" time="1:13:12 PM" user="Tim" />
    <Modified change="31" date="Monday, 23 February 2015" time="1:23:33 PM" user="Tim" />
    <Modified change="32" date="Monday, 23 February 2015" time="1:54:47 PM" user="Tim" />
    <Modified change="33" date="Monday, 23 February 2015" time="2:04:49 PM" user="Tim" />
    <Modified change="34" date="Monday, 23 February 2015" time="2:14:55 PM" user="Tim" />
    <Modified change="35" date="Monday, 23 February 2015" time="2:25:03 PM" user="Tim" />
    <Modified change="36" date="Monday, 23 February 2015" time="3:46:44 PM" user="Tim" />
    <Modified change="37" date="Monday, 23 February 2015" time="3:50:52 PM" user="Tim" />
    <Modified change="38" date="Monday, 23 February 2015" time="4:00:56 PM" user="Tim" />
    <Modified change="39" date="Monday, 23 February 2015" time="4:10:56 PM" user="Tim" />
    <Modified change="40" date="Monday, 23 February 2015" time="4:20:57 PM" user="Tim" />
    <Modified change="41" date="Monday, 23 February 2015" time="4:30:57 PM" user="Tim" />
    <Modified change="42" date="Monday, 23 February 2015" time="5:30:58 PM" user="Tim" />
    <Modified change="43" date="Monday, 23 February 2015" time="5:41:05 PM" user="Tim" />
    <Modified change="44" date="Monday, 23 February 2015" time="6:01:14 PM" user="Tim" />
    <Modified change="45" date="Monday, 23 February 2015" time="6:11:14 PM" user="Tim" />
    <Modified change="46" date="Monday, 23 February 2015" time="8:31:56 PM" user="Tim" />
    <Modified change="47" date="Monday, 23 February 2015" time="8:48:42 PM" user="Tim" />
    <Modified change="48" date="Monday, 23 February 2015" time="8:53:19 PM" user="Tim" />
    <Modified change="49" date="Monday, 23 February 2015" time="9:03:19 PM" user="Tim" />
    <Modified change="50" date="Tuesday, 24 February 2015" time="7:02:38 AM" user="Tim" />
    <Modified change="51" date="Tuesday, 24 February 2015" time="7:27:14 AM" user="Tim" />
    <Modified change="52" date="Tuesday, 24 February 2015" time="10:00:12 AM" user="Tim" />
    <Modified change="53" date="Friday, 27 February 2015" time="6:52:10 AM" user="Tim" />
    <Modified change="54" date="Monday, 2 March 2015" time="12:39:47 PM" user="Tim" />
    <Modified change="55" date="Tuesday, 10 March 2015" time="9:31:54 AM" user="Tim" />
    <Modified change="56" date="Wednesday, 1 April 2015" time="1:38:45 PM" user="matt" />
  </History>
  <Version grokVersion="8.61" saveVersion="1.34" realVersion="8.75" />
</FlipProject>