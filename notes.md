> myacc bff

 1. myaccount bff [git]
 2. myaccount bff deploy [git]
 3. myaccount bff [bk]
 4. myaccount bff deploy [bk]
 5. perform another search
 6. exit


> 1

1. copy to clipboard
2. open in browser
3. perform another search
4. exit

> 2

search criteria:
lob:squad:project:system:action
> rca:myacc:bff:git
> myaccount:bff:git
> myaccount::git
> :bff:git
> rca:myacc:bff#stg

> rca:myacc:bff:stg:rattic
> myacc:bff:rattic


 data Tag = Tag String

data Entry = Entry { uri :: URI, tags :: [Tag]}

Entry {
    uri = "https://"
    tags = [Tag "RCA", Tag "Personalisation", Tag "Github", Tag "BFF",]
}

> :rca:*my*:bff:git
> :person*:git
> :bff

{
    [{
        url: "https://........",
        tags: ["myaccount","rca","personalisation","github","bff"]
    },]
}

rca.myaccount.bff.personalisation.github="https://........"
rca.myaccount.bff-deploy.personalisation.github="https://........"
rca.myaccount.bff.personalisation.buildkite="https://........"
rca.myaccount.bff-deploy.personalisation.buildkite="https://........"
rca.myaccount.bff.personalisation.staging="https://........"
rca.myaccount.bff.personalisation.production="https://........"
rca.myaccount.bff.personalisation.staging.rattic="https://........"

rca.myaccount.ui.personalisation.github="https://........"
rca.myaccount.ui-deploy.personalisation.github="https://........"
rca.myaccount.ui.personalisation.buildkite="https://........"
rca.myaccount.ui-deploy.personalisation.buildkite="https://........"
rca.myaccount.ui.personalisation.staging="https://........"
rca.myaccount.ui.personalisation.production="https://........"

--------------------
data Config = Config { tags :: [String], uri :: String } deriving (Show, Generic)
instance FromJSON Config
Y.decodeFileEither "sample/recite.yml" :: IO (Either Y.ParseException [Config])